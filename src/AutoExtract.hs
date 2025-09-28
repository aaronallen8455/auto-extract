{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
module AutoExtract
  ( plugin
  ) where

import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Writer.CPS as W
import           Control.Exception (catch, throw)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import qualified Data.Char as Char
import           Data.Foldable
import qualified Data.Generics as Syb
import           Data.IORef
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.String
import qualified GHC.IsList as IsList
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
import qualified Language.Haskell.GHC.ExactPrint.Utils as EP

import qualified AutoExtract.GhcFacade as Ghc

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.driverPlugin = \_opts env -> updateHscEnv env
  }

type ExtractedDecls = Map.Map BS.ByteString Extraction

data Extraction = Extraction
  { argNames :: [Ghc.OccName]
  , extractedType :: Maybe (Ghc.HsType Ghc.GhcPs)
  }

updateHscEnv :: Ghc.HscEnv -> IO Ghc.HscEnv
updateHscEnv hscEnv = do
  pure hscEnv
    { Ghc.hsc_hooks = (Ghc.hsc_hooks hscEnv) { Ghc.runPhaseHook = Just installHooks } }
  where
    installHooks :: Ghc.PhaseHook
    installHooks = Ghc.PhaseHook $ \phase -> case phase of
      Ghc.T_Hsc env modSum -> catch (runPhaseOrExistingHook phase)
        (\(Ghc.SourceError msgs) ->
          case any atParseErr $ Ghc.getMessages msgs of
            True | Just updatedBuffer <- updateBuffer =<< Ghc.ms_hspp_buf modSum
              -> do
              extractedNamesRef <- newIORef Map.empty
              let innerPlugin = mkInnerPlugin hscEnv extractedNamesRef
                  updatedModSum = modSum { Ghc.ms_hspp_buf = Just updatedBuffer }
                  staticPlugin = Ghc.StaticPlugin
                    { Ghc.spPlugin = Ghc.PluginWithArgs innerPlugin []
#if MIN_VERSION_ghc(9,12,0)
                    , Ghc.spInitialised = True
#endif
                    }
                  newEnv = env
                    { Ghc.hsc_plugins = let plugins = Ghc.hsc_plugins hscEnv in plugins
                      { Ghc.staticPlugins = staticPlugin : Ghc.staticPlugins plugins }
                    }
              runPhaseOrExistingHook (Ghc.T_Hsc newEnv updatedModSum)
            _ -> throw $ Ghc.SourceError msgs
        )
      _ -> runPhaseOrExistingHook phase

    runPhaseOrExistingHook :: Ghc.TPhase res -> IO res
    runPhaseOrExistingHook =
      maybe Ghc.runPhase (\(Ghc.PhaseHook h) -> h)
        . Ghc.runPhaseHook $ Ghc.hsc_hooks hscEnv

    atParseErr msgEnv =
      case Ghc.errMsgDiagnostic msgEnv of
        Ghc.GhcPsMessage (Ghc.PsErrParse "@" _) -> True
        _ -> False

mkInnerPlugin :: Ghc.HscEnv -> IORef (Map.Map Ghc.Name [Ghc.Name]) -> Ghc.Plugin
mkInnerPlugin hscEnv extractedNamesRef = Ghc.defaultPlugin
  { Ghc.parsedResultAction = \_ _ result -> do
      pure $ rewriteToLet result
  , Ghc.renamedResultAction = \_ gblEnv grp -> do
      let (newNames, newGrp) = performExtractions gblEnv grp
      liftIO $ modifyIORef extractedNamesRef (Map.union (Map.fromList newNames))
      pure (gblEnv, newGrp)
  , Ghc.typeCheckResultAction = \_ tcModSum gblEnv -> do
      extractedNames <- liftIO $ readIORef extractedNamesRef
      let dynFlags = Ghc.ms_hspp_opts tcModSum `Ghc.gopt_set` Ghc.Opt_KeepRawTokenStream
      extractionParams <- Map.mapKeys nameToBS <$>
        Map.traverseWithKey
          (\nm args -> do
            ty <- Ghc.idType <$> Ghc.tcLookupId nm
            -- Converting Type to HsType would be tedious so instead we
            -- pretty print the Type and run it through the type parser.
            let tySDoc = Ghc.pprSigmaType ty
                sdocCtxt = (Ghc.initDefaultSDocContext dynFlags)
                  { Ghc.sdocLineLength = 5000 }
                tyStr = Ghc.renderWithContext sdocCtxt tySDoc
                mHsTy = either (const Nothing) Just
                      $ EP.parseType dynFlags "" tyStr
            pure Extraction
              { argNames = Ghc.occName <$> args
              , extractedType = Ghc.unLoc <$> mHsTy
              }
          )
          extractedNames
      let extractErr =
            let fn = fromMaybe "<UNKNOWN>" $ Ghc.ml_hs_file (Ghc.ms_location tcModSum)
             in Ghc.mkPlainErrorMsgEnvelope
                  (Ghc.mkGeneralSrcSpan $ fromString fn)
                  (Ghc.ghcUnknownMessage ExtractDiag)

      case Ghc.ml_hs_file (Ghc.ms_location tcModSum) of
        Nothing -> pure gblEnv
        Just filePath -> do
          liftIO $ prepareSourceForParsing filePath
          parseResult <- liftIO $ parseModule hscEnv dynFlags filePath
          case parseResult of
            (Right parsedMod, usesCpp) ->
              liftIO $ modifyModule parsedMod usesCpp extractionParams filePath
            (Left _, _) -> pure ()
          Ghc.throwOneError extractErr
  }

updateBuffer :: Ghc.StringBuffer -> Maybe Ghc.StringBuffer
updateBuffer = fmap Ghc.stringBufferFromByteString . rewriteRawExtract . stringBufferToBS

rewriteRawExtract :: BS.ByteString -> Maybe BS.ByteString
rewriteRawExtract = update False
  where
    update matchFound bs =
      case BS.breakSubstring "EXTRACT@" bs of
        (before, "") -> if matchFound then Just before else Nothing
        (before, match) -> do
          let name = BS8.takeWhile (not . Char.isSpace) $ BS.drop 8 match
          guard . not $ BS.null name
          let rest = BS.drop (8 + BS.length name) match
              expr = "EXTRACT \"" <> name <> "\""
          newRest <- update True rest
          Just $ before <> expr <> newRest

stringBufferToBS :: Ghc.StringBuffer -> BS.ByteString
stringBufferToBS Ghc.StringBuffer {Ghc.buf = buf, Ghc.len = len} =
  BS.BS buf len

pattern ExtractPat :: Ghc.FastString -> Ghc.LHsExpr Ghc.GhcPs -> Ghc.HsExpr Ghc.GhcPs
pattern ExtractPat bnd body
  <- Ghc.HsApp _
       (Ghc.L _
         (Ghc.HsApp _
           (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ (nameToBS -> "EXTRACT"))))
           (Ghc.L _ (Ghc.HsLit _ (Ghc.HsString _ bnd)))
         )
       )
       (removeParens -> body)

removeParens :: Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
removeParens (Ghc.L l (Ghc.HsPar _ (Ghc.L _ x))) = Ghc.L l (doIndentCorrect x)
-- removeParens (Ghc.L l (Ghc.HsPar _ (Ghc.L _ x))) = -- Ghc.L l (doIndentCorrect x)
--   let addCol = \case
--         Ghc.EpaDelta (Ghc.DifferentLine r c) a ->
--           Ghc.EpaDelta (Ghc.DifferentLine r (c + 1)) a
--         a -> a
--    in Ghc.L l { Ghc.entry = addCol (Ghc.entry l) } x
removeParens x = x

-- | Older versions exact-print have an indentation bug involving do statements
doIndentCorrect :: Ghc.HsExpr Ghc.GhcPs -> Ghc.HsExpr Ghc.GhcPs
#if !MIN_VERSION_ghc(9,12,0)
doIndentCorrect (Ghc.HsDo ann t (Ghc.L l2 s)) =
  let addCol = \case
        Ghc.EpaDelta (Ghc.DifferentLine r c) a ->
          Ghc.EpaDelta (Ghc.DifferentLine r (c + 1)) a
        x -> x
   in Ghc.HsDo ann t (Ghc.L l2 {Ghc.entry = addCol (Ghc.entry l2)} s)
#endif
doIndentCorrect x = x

nameToBS :: Ghc.HasOccName a => a -> BS.ByteString
nameToBS = Ghc.bytesFS . Ghc.occNameFS . Ghc.occName

rewriteToLet :: Ghc.ParsedResult -> Ghc.ParsedResult
rewriteToLet result =
  let prm = Ghc.parsedResultModule result in result
        { Ghc.parsedResultModule = prm
          { Ghc.hpm_module = rewrite $ Ghc.hpm_module prm
          }
        }
  where
    rewrite = Syb.everywhere $ Syb.mkT appCase
    appCase :: Ghc.HsExpr Ghc.GhcPs -> Ghc.HsExpr Ghc.GhcPs
    appCase = \case
      ExtractPat bnd body -> letExpr bnd body
      x -> x

    letExpr bnd body =
      let bndName = Ghc.mkVarUnqual $ bnd <> "_EXTRACT"
          mg :: Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
#if MIN_VERSION_ghc(9,12,0)
          mg = Ghc.MG Ghc.FromSource
                 (Ghc.L Ghc.noSrcSpanA
                   [Ghc.noLocA $ Ghc.Match
                     Ghc.noExtField
                     (Ghc.FunRhs (Ghc.noLocA bndName) Ghc.Prefix Ghc.SrcLazy Ghc.noAnn)
                     (Ghc.noLocA [])
                     grhss
                   ]
                 )
#else
          mg = Ghc.MG Ghc.FromSource
                 (Ghc.L Ghc.noSrcSpanA
                   [Ghc.noLocA $ Ghc.Match
                     []
                     (Ghc.FunRhs (Ghc.noLocA bndName) Ghc.Prefix Ghc.SrcLazy)
                     []
                     grhss
                   ]
                 )
#endif
          grhss :: Ghc.GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
          grhss = Ghc.GRHSs Ghc.emptyComments
                    [Ghc.noLocA $ Ghc.GRHS Ghc.noSrcSpanA [] body]
                    (Ghc.EmptyLocalBinds Ghc.noExtField)
          expr :: Ghc.HsExpr Ghc.GhcPs
          expr = Ghc.HsLet Ghc.noAnn
            (Ghc.HsValBinds Ghc.noSrcSpanA
               (Ghc.ValBinds Ghc.NoAnnSortKey
                  [ Ghc.noLocA $ Ghc.FunBind Ghc.noExtField (Ghc.noLocA bndName) mg ]
                  []
               )
            )
            (Ghc.noLocA $ Ghc.HsVar Ghc.noExtField (Ghc.noLocA bndName))
       in expr

-- | Custom scheme that allows a transformation to inspect the context within
-- a bottom up traversal.
everywhereCtx
  :: forall w. Monoid w
  => (w -> Syb.GenericM (W.Writer w))
  -> Syb.GenericM (W.Writer w)
everywhereCtx f = go where
  go :: Syb.GenericM (W.Writer w)
  go x = do
    (x', w) <- W.listen $ Syb.gmapM go x
    f w x'

performExtractions
  :: Ghc.TcGblEnv
  -> Ghc.HsGroup Ghc.GhcRn
  -> ([(Ghc.Name, [Ghc.Name])], Ghc.HsGroup Ghc.GhcRn)
performExtractions gblEnv grp =
  case Ghc.hs_valds grp of
    Ghc.XValBindsLR (Ghc.NValBinds bndTups sigs)
      | let extracted = bndExtract <$> bndTups ->
        ( foldMap snd extracted
        , grp
          { Ghc.hs_valds = Ghc.XValBindsLR (Ghc.NValBinds (fst <$> extracted) sigs) }
        )
    _ -> ([], grp)
  where
    bndExtract
      :: (Ghc.RecFlag, Ghc.LHsBinds Ghc.GhcRn)
      -> ((Ghc.RecFlag, Ghc.LHsBinds Ghc.GhcRn), [(Ghc.Name, [Ghc.Name])])
    bndExtract (_, bnds) =
      let (newBnds, newNames) = foldMap extract bnds
       in ((Ghc.Recursive, IsList.fromList newBnds), newNames)

    extract :: Ghc.LHsBind Ghc.GhcRn -> ([Ghc.LHsBind Ghc.GhcRn], [(Ghc.Name, [Ghc.Name])])
    extract (Ghc.L loc bind) =
      let (updated, (newBinds, newNames)) = W.runWriter $
            everywhereCtx
              (\w ->
                let newDeclNames = fst <$> snd w
                 in Syb.mkM (rewriteAndExtract newDeclNames) `Syb.extM` addFVs newDeclNames
              )
              bind
          rewriteAndExtract
            :: [Ghc.Name]
            -> Ghc.HsExpr Ghc.GhcRn
            -> W.Writer ([Ghc.HsBind Ghc.GhcRn], [(Ghc.Name, [Ghc.Name])]) (Ghc.HsExpr Ghc.GhcRn)
          rewriteAndExtract newDeclNames = \case
#if MIN_VERSION_ghc(9,12,0)
                Ghc.HsLet _
                  (Ghc.HsValBinds _
                     (Ghc.XValBindsLR
                       (Ghc.NValBinds
                         [ ( _recFlag
                           , [ Ghc.L _
                               (Ghc.FunBind freeVars (Ghc.L _ bndName)
                                 (Ghc.MG Ghc.FromSource
                                   (Ghc.L _
                                     [Ghc.L _
                                       (Ghc.Match
                                         _
                                         _
                                         _
                                         grhss@(Ghc.GRHSs _
                                           [Ghc.L _ (Ghc.GRHS _ [] _)]
                                           _
                                         )
                                       )
                                     ]
                                   )
                                 )
                               )
                             ]
                           )
                         ]
                         []
                       )
                     )
                  )
#else
                Ghc.HsLet _
                  (Ghc.HsValBinds _
                     (Ghc.XValBindsLR
                       (Ghc.NValBinds
                         [ ( _recFlag
                           , [ Ghc.L _
                                 (Ghc.FunBind freeVars (Ghc.L _ bndName)
                                   (Ghc.MG Ghc.FromSource
                                     (Ghc.L _
                                       [Ghc.L _
                                         (Ghc.Match
                                           _
                                           _
                                           _
                                           grhss@(Ghc.GRHSs _
                                             [Ghc.L _ (Ghc.GRHS _ [] _)]
                                             _
                                           )
                                         )
                                       ]
                                     )
                                   )
                                )
                             ]
                           )
                         ]
                         []
                       )
                     )
                  )
#endif
                  _
                    | Just occNameBS <- BS.stripSuffix "_EXTRACT" $ nameToBS bndName
                    -> let args = Ghc.nameSetElemsStable
                                . Ghc.delFVs newDeclNames
                                $ freeVars `Ghc.minusNameSet` topLevelNames
                           topLvlName = Ghc.tidyNameOcc bndName (Ghc.mkVarOccFS $ Ghc.mkFastStringByteString occNameBS)
                           newBind =
#if MIN_VERSION_ghc(9,12,0)
                             Ghc.FunBind freeVars (Ghc.noLocA topLvlName) $
                               Ghc.MG Ghc.FromSource
                                 (Ghc.L Ghc.noSrcSpanA
                                   [Ghc.noLocA $ Ghc.Match
                                     Ghc.noExtField
                                     (Ghc.FunRhs (Ghc.noLocA topLvlName) Ghc.Prefix Ghc.SrcLazy Ghc.noAnn)
                                     (Ghc.noLocA $ Ghc.noLocA . Ghc.VarPat Ghc.noExtField . Ghc.noLocA <$> args)
                                     grhss
                                   ]
                                 )
#else
                             Ghc.FunBind freeVars (Ghc.noLocA topLvlName) $
                               Ghc.MG Ghc.FromSource
                                 (Ghc.L Ghc.noSrcSpanA
                                   [Ghc.noLocA $ Ghc.Match
                                     []
                                     (Ghc.FunRhs (Ghc.noLocA topLvlName) Ghc.Prefix Ghc.SrcLazy)
                                     (Ghc.noLocA . Ghc.VarPat Ghc.noExtField . Ghc.noLocA <$> args)
                                     grhss
                                   ]
                                 )
#endif
                           newExpr =
                             foldl'
                               (\acc arg ->
                                 Ghc.HsApp
                                   Ghc.noExtField
                                   (Ghc.noLocA acc)
                                   (Ghc.noLocA $ Ghc.HsVar Ghc.noExtField (Ghc.noLocA arg)))
                               (Ghc.HsVar Ghc.noExtField (Ghc.noLocA topLvlName))
                               args
                        in W.writer (newExpr, ([newBind], [(topLvlName, args)]))

                x -> pure x

          addFVs :: Monad m
                 => [Ghc.Name]
                 -> Ghc.HsBind Ghc.GhcRn
                 -> m (Ghc.HsBind Ghc.GhcRn)
          addFVs newDeclNames = \case
                Ghc.FunBind fvs a b ->
                  pure $ Ghc.FunBind (Ghc.extendNameSetList fvs newDeclNames) a b
                Ghc.PatBind fvs a b c ->
                  pure $ Ghc.PatBind (Ghc.extendNameSetList fvs newDeclNames) a b c
                x -> pure x

       in (Ghc.L loc updated : (Ghc.noLocA <$> reverse newBinds), newNames)

    topLevelNames :: Ghc.NameSet
    topLevelNames = foldMap (fold . fst) $ Ghc.tcg_dus gblEnv

-- When modifying parser input, convert EXTRACT@foo to foo_EXTRACT
modifyParsedDecls :: ExtractedDecls -> [Ghc.LHsDecl Ghc.GhcPs] -> [Ghc.LHsDecl Ghc.GhcPs]
modifyParsedDecls extrDecls = foldMap go
  where
    go decl =
      let (updDecl, newDecls) = W.runWriter $ Syb.everywhereM (Syb.mkM extract) decl
       in updDecl : newDecls
    extract :: Ghc.HsExpr Ghc.GhcPs -> W.Writer [Ghc.LHsDecl Ghc.GhcPs] (Ghc.HsExpr Ghc.GhcPs)
    extract = \case
      ExtractPat bnd body
        | Just inputs <- Map.lookup (Ghc.bytesFS bnd) extrDecls
        -> decls bnd body inputs
      x -> pure x

    decls bnd body inputs =
      let rdrName = Ghc.mkRdrUnqual $ Ghc.mkVarOccFS bnd
          arNames = Ghc.L Ghc.anchorD1 . Ghc.mkRdrUnqual <$> argNames inputs
          callsite = foldl'
            (\acc arg ->
              Ghc.HsApp
                Ghc.noExtField
                (Ghc.noLocA acc)
                (Ghc.L Ghc.noAnn $ Ghc.HsVar Ghc.noExtField arg)
            )
            (Ghc.HsVar Ghc.noExtField $ Ghc.noLocA rdrName)
            arNames
          grhss :: Ghc.GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
#if MIN_VERSION_ghc(9,12,0)
          grhss = Ghc.GRHSs Ghc.emptyComments
                    [Ghc.noLocA $ Ghc.GRHS
                      (Ghc.EpAnn EP.d0
                        (Ghc.GrhsAnn Nothing (Left (Ghc.EpTok EP.d1)))
                        Ghc.emptyComments
                      )
                      []
                      body
                    ]
                    (Ghc.EmptyLocalBinds Ghc.noExtField)
#else
          grhss = Ghc.GRHSs Ghc.emptyComments
                    [Ghc.L Ghc.anchorD1 $ Ghc.GRHS
                      (Ghc.EpAnn EP.d0
                        (Ghc.GrhsAnn Nothing (Ghc.AddEpAnn Ghc.AnnEqual EP.d0)) -- (Left (Ghc.EpTok EP.d1)))
                        Ghc.emptyComments
                      )
                      []
                      body
                    ]
                    (Ghc.EmptyLocalBinds Ghc.noExtField)
#endif
          newDecl :: Ghc.LHsDecl Ghc.GhcPs
#if MIN_VERSION_ghc(9,12,0)
          newDecl = Ghc.L (Ghc.diffLine 1 0) $
            Ghc.ValD Ghc.noExtField $ Ghc.FunBind Ghc.noExtField (Ghc.noLocA rdrName) $
              Ghc.MG Ghc.FromSource
                (Ghc.L Ghc.noSrcSpanA
                  [Ghc.L Ghc.noAnn $ Ghc.Match
                    Ghc.noExtField
                    (Ghc.FunRhs (Ghc.noLocA rdrName) Ghc.Prefix Ghc.SrcLazy Ghc.noAnn)
                    (Ghc.noLocA $ Ghc.L (Ghc.diffLine 0 1) . Ghc.VarPat Ghc.noExtField <$> arNames)
                    grhss
                  ]
                )
#else
          newDecl = Ghc.L (Ghc.diffLine 1 1) $
            Ghc.ValD Ghc.noExtField $ Ghc.FunBind Ghc.noExtField (Ghc.L EP.noAnnSrcSpanDP0 rdrName) $
              Ghc.MG Ghc.FromSource
                (Ghc.L EP.noAnnSrcSpanDP0
                  [Ghc.L Ghc.noAnn $ Ghc.Match
                    []
                    (Ghc.FunRhs (Ghc.L EP.noAnnSrcSpanDP0 rdrName) Ghc.Prefix Ghc.SrcLazy)
                    (Ghc.L EP.noAnnSrcSpanDP0 . Ghc.VarPat Ghc.noExtField <$> arNames)
                    grhss
                  ]
                )
#endif
          mSig :: Maybe (Ghc.LHsDecl Ghc.GhcPs)
          mSig = do
            hsType <- extractedType inputs
            Just $ Ghc.L (Ghc.diffLine 2 0) $ Ghc.SigD Ghc.noExtField $
              Ghc.TypeSig
                  (Ghc.AnnSig
#if MIN_VERSION_ghc(9,12,0)
                    (Ghc.EpUniTok EP.d1 Ghc.NormalSyntax)
                    Nothing
                    Nothing
#else
                    (Ghc.AddEpAnn Ghc.AnnDcolon EP.d1)
                    []
#endif
                  )
                  [Ghc.noLocA rdrName] $
                Ghc.HsWC Ghc.noExtField $ Ghc.L Ghc.anchorD1 $
                  Ghc.HsSig Ghc.noExtField Ghc.mkHsOuterImplicit (Ghc.noLocA hsType)
       in W.writer (callsite, maybe id (:) mSig [newDecl])

-- | Parse the given module file. Accounts for CPP comments
parseModule
  :: Ghc.HscEnv
  -> Ghc.DynFlags
  -> FilePath
  -> IO (EP.ParseResult Ghc.ParsedSource, Bool)
parseModule env dynFlags filePath = EP.ghcWrapper Paths.libdir $ do
  Ghc.setSession env { Ghc.hsc_dflags = dynFlags }
  res <- EP.parseModuleEpAnnsWithCppInternal EP.defaultCppOptions dynFlags filePath
  let eCppComments = fmap (\(c, _, _) -> c) res
      hasCpp = case eCppComments of
                 Right cs -> not $ null cs
                 _ -> False
  pure
    ( liftA2 EP.insertCppComments
        (EP.postParseTransform res)
        eCppComments
    , hasCpp
    )

prepareSourceForParsing
  :: FilePath
  -> IO ()
prepareSourceForParsing filePath = do
  content <- BS.readFile filePath
  traverse_ (BS.writeFile filePath) (rewriteRawExtract content)

modifyModule
  :: Ghc.ParsedSource
  -> Bool
  -> ExtractedDecls
  -> FilePath
  -> IO ()
modifyModule parsedMod usesCpp extractedDecls filePath = do
  let ast = EP.makeDeltaAst parsedMod
      updatedDecls = modifyParsedDecls extractedDecls (EP.hsDecls ast)
      updatedMod = EP.replaceDecls ast updatedDecls
  -- If the source contains CPP, newlines are appended
  -- to the end of the file when exact printing. The simple
  -- solution is to remove trailing newlines after exact printing
  -- if the source contains CPP comments.
  let removeTrailingNewlines
        | usesCpp =
            reverse . ('\n' :) . dropWhile (== '\n') . reverse
        | otherwise = id
      printed = removeTrailingNewlines $ EP.exactPrint updatedMod
  writeFile filePath printed

-- | Diagnostic thrown when extraction occurs
data ExtractDiag = ExtractDiag

instance Ghc.Diagnostic ExtractDiag where
  type DiagnosticOpts ExtractDiag = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "Module updated by auto-extract, compilation aborted"
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif
