{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module AutoExtract
  ( plugin
  ) where

import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (catch, throw)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import qualified Data.Char as Char
import           Data.Foldable
import qualified Data.Generics as Syb
import           Data.IORef
import qualified Data.Map.Strict as Map
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
  hasExtractRef <- newIORef False
  extractedNamesRef <- newIORef Map.empty
  let innerPlugin = Ghc.defaultPlugin
        { Ghc.parsedResultAction = \_ _ result -> do
            hasExtract <- liftIO $ readIORef hasExtractRef
            if hasExtract
            then pure $ rewriteToLet result
            else pure result
        , Ghc.renamedResultAction = \_ gblEnv grp -> do
            hasExtract <- liftIO $ readIORef hasExtractRef
            if hasExtract
            then do
              let (newNames, newGrp) = performExtractions gblEnv grp
              liftIO $ modifyIORef extractedNamesRef (Map.union (Map.fromList newNames))
              pure (gblEnv, newGrp)
            else pure (gblEnv, grp)
        , Ghc.typeCheckResultAction = \_ modSum gblEnv -> do
            extractedNames <- liftIO $ readIORef extractedNamesRef
            let dynFlags = Ghc.ms_hspp_opts modSum `Ghc.gopt_set` Ghc.Opt_KeepRawTokenStream
            extractionParams <- Map.mapKeys (Ghc.bytesFS . Ghc.occNameFS . Ghc.occName) <$>
              Map.traverseWithKey
                (\nm args -> do
                  ty <- Ghc.idType <$> Ghc.tcLookupId nm
                  -- Converting Type to HsType would be tedious so instead we
                  -- pretty print the Type and run it through the type parser.
                  let tySDoc = Ghc.pprSigmaType ty
                      sdocCtxt = Ghc.initDefaultSDocContext dynFlags
                      tyStr = Ghc.renderWithContext sdocCtxt tySDoc
                      mHsTy = either (const Nothing) Just
                            $ EP.parseType dynFlags "" tyStr
                  pure Extraction
                    { argNames = Ghc.occName <$> args
                    , extractedType = Ghc.unLoc <$> mHsTy
                    }
                )
                extractedNames

            case Ghc.ml_hs_file (Ghc.ms_location modSum) of
              Nothing -> pure gblEnv -- TODO throw error
              Just filePath -> do
                liftIO $ prepareSourceForParsing filePath
                parseResult <- liftIO $ parseModule hscEnv dynFlags filePath
                case parseResult of
                  (Right parsedMod, usesCpp) -> do
                    liftIO $ modifyModule parsedMod usesCpp extractionParams filePath
                    pure gblEnv -- TODO throw error
                  (Left _, _) -> pure gblEnv -- TODO throw error
        }
  let staticPlugin = Ghc.StaticPlugin
        { Ghc.spPlugin = Ghc.PluginWithArgs innerPlugin []
        , Ghc.spInitialised = True
        }
  pure hscEnv
    { Ghc.hsc_hooks = (Ghc.hsc_hooks hscEnv) { Ghc.runPhaseHook = Just $ installHooks hasExtractRef }
    , Ghc.hsc_plugins = let plugins = Ghc.hsc_plugins hscEnv in plugins
      { Ghc.staticPlugins = staticPlugin : Ghc.staticPlugins plugins
      }
    }
  where
    installHooks :: IORef Bool -> Ghc.PhaseHook
    installHooks hasExtractRef = Ghc.PhaseHook $ \phase -> case phase of
      Ghc.T_Hsc env modSum -> catch (runPhaseOrExistingHook phase)
        (\(Ghc.SourceError msgs) ->
          case any atParseErr $ Ghc.getMessages msgs of
            True | Just updatedBuffer <- updateBuffer =<< Ghc.ms_hspp_buf modSum
              -> do
              let updatedModSum = modSum { Ghc.ms_hspp_buf = Just updatedBuffer }
              -- Set a ref indicating this module has an extraction
              writeIORef hasExtractRef True
              runPhaseOrExistingHook (Ghc.T_Hsc env updatedModSum)
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

updateBuffer :: Ghc.StringBuffer -> Maybe Ghc.StringBuffer
updateBuffer = fmap Ghc.stringBufferFromByteString . update False . stringBufferToBS
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
      -- TODO dollar application
      Ghc.HsApp _
        (Ghc.L _
          (Ghc.HsApp _
            (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ v)))
            (Ghc.L _ (Ghc.HsLit _ (Ghc.HsString _ bnd)))
          )
        )
        body
          | Ghc.occNameFS (Ghc.occName v) == "EXTRACT"
          ->
            let bndName = Ghc.mkVarUnqual $ bnd <> "_EXTRACT"
                mg :: Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
                mg = Ghc.MG Ghc.FromSource
                       (Ghc.L Ghc.noSrcSpanA
                         [Ghc.noLocA $ Ghc.Match
                           Ghc.noExtField
                           (Ghc.FunRhs (Ghc.noLocA bndName) Ghc.Prefix Ghc.SrcLazy Ghc.noAnn)
                           (Ghc.noLocA [])
                           grhss
                         ]
                       )
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
      x -> x

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
       in ((Ghc.Recursive, newBnds), newNames)

    extract :: Ghc.LHsBind Ghc.GhcRn -> ([Ghc.LHsBind Ghc.GhcRn], [(Ghc.Name, [Ghc.Name])])
    extract (Ghc.L loc bind) =
      let ((newBinds, newNames), updated) = Syb.everywhereM (Syb.mkM go) bind
          -- TODO use CPS writer
          go :: Ghc.HsExpr Ghc.GhcRn
             -> (([Ghc.HsBind Ghc.GhcRn], [(Ghc.Name, [Ghc.Name])]), Ghc.HsExpr Ghc.GhcRn)
          go = \case
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
                  _
                    | Just occNameBS <- BS.stripSuffix "_EXTRACT" . Ghc.bytesFS . Ghc.occNameFS $ Ghc.occName bndName
                    -> let args = Ghc.nameSetElemsStable
                                $ freeVars `Ghc.minusNameSet` topLevelNames
                           topLvlName = Ghc.mkExternalName (Ghc.nameUnique bndName) (Ghc.tcg_mod gblEnv) (Ghc.mkVarOccFS $ Ghc.mkFastStringByteString occNameBS) Ghc.noSrcSpan
                           newBind =
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
                           newExpr =
                             foldl'
                               (\acc arg ->
                                 Ghc.HsApp
                                   Ghc.noExtField
                                   (Ghc.noLocA acc)
                                   (Ghc.noLocA $ Ghc.HsVar Ghc.noExtField (Ghc.noLocA arg)))
                               (Ghc.HsVar Ghc.noExtField (Ghc.noLocA topLvlName))
                               args
                        in (([newBind], [(topLvlName, args)]), newExpr)

                x -> pure x

       in (Ghc.L loc updated : (Ghc.noLocA <$> newBinds), newNames)

    topLevelNames :: Ghc.NameSet
    topLevelNames = foldMap (fold . fst) $ Ghc.tcg_dus gblEnv

-- When modifying parser input, convert EXTRACT@foo to foo_EXTRACT
modifyParsedDecls :: ExtractedDecls -> [Ghc.LHsDecl Ghc.GhcPs] -> [Ghc.LHsDecl Ghc.GhcPs]
modifyParsedDecls extrDecls = foldMap go
  where
    go decl =
      let (newDecls, updDecl) = Syb.everywhereM (Syb.mkM extract) decl
       in updDecl : newDecls
    extract :: Ghc.HsExpr Ghc.GhcPs -> ([Ghc.LHsDecl Ghc.GhcPs], Ghc.HsExpr Ghc.GhcPs)
    extract = \case
      Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ n))) body
        | Just realName <- BS.stripSuffix "_EXTRACT" . Ghc.bytesFS . Ghc.occNameFS $ Ghc.occName n
        , Just inputs <- Map.lookup realName extrDecls
        , let rdrName = Ghc.mkRdrUnqual . Ghc.mkVarOccFS $ Ghc.mkFastStringByteString realName
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
              mSig :: Maybe (Ghc.LHsDecl Ghc.GhcPs)
              mSig = do
                hsType <- extractedType inputs
                Just $ Ghc.L (Ghc.diffLine 2 0) $ Ghc.SigD Ghc.noExtField $
                  Ghc.TypeSig
                      (Ghc.AnnSig (Ghc.EpUniTok EP.d1 Ghc.NormalSyntax) Nothing Nothing)
                      [Ghc.noLocA rdrName] $
                    Ghc.HsWC Ghc.noExtField $ Ghc.L Ghc.anchorD1 $
                      Ghc.HsSig Ghc.noExtField Ghc.mkHsOuterImplicit (Ghc.noLocA hsType)
        -> (maybe id (:) mSig [newDecl], callsite)
      x -> (mempty, x)

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
  let update bs =
        case BS.breakSubstring "EXTRACT@" bs of
          (before, "") -> before
          (before, match) ->
            let name = BS8.takeWhile (not . Char.isSpace) $ BS.drop 8 match
                rest = BS.drop (8 + BS.length name) match
                expr = name <> "_EXTRACT"
            in before <> expr <> update rest
  BS.writeFile filePath (update content)

modifyModule
  :: Ghc.ParsedSource
  -> Bool
  -> ExtractedDecls
  -> FilePath
  -> IO ()
modifyModule parsedMod usesCpp extractedDecls filePath = do
  let ast = EP.makeDeltaAst parsedMod
      updatedDecls = modifyParsedDecls extractedDecls (EP.hsDecls ast)
      updatedMod = (\m -> m {Ghc.hsmodDecls = updatedDecls}) <$> parsedMod
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
