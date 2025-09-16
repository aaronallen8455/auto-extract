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
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP

import qualified AutoExtract.GhcFacade as Ghc

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.driverPlugin = \_opts env -> updateHscEnv env
  }

type ExtractedDecls = Map.Map Ghc.OccName Extraction

data Extraction = Extraction
  { argNames :: [Ghc.OccName]
  , extractedType :: Maybe (Ghc.HsType Ghc.GhcPs)
  -- , extractedName :: Ghc.OccName
  }

updateHscEnv :: Ghc.HscEnv -> IO Ghc.HscEnv
updateHscEnv hscEnv = do
  hasExtractRef <- newIORef False
  extractedNamesRef <- newIORef Map.empty -- Ghc.emptyNameSet
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
        , Ghc.typeCheckResultAction = \_ _ gblEnv -> do
            extractedNames <- liftIO $ readIORef extractedNamesRef
            liftIO $ putStrLn $ Ghc.showSDocUnsafe $ Ghc.ppr extractedNames
            liftIO $ putStrLn $ Ghc.showSDocUnsafe $ Ghc.ppr $ Ghc.tcg_binds gblEnv
            ids <- traverse Ghc.tcLookupId $ Map.keys extractedNames
            liftIO $ putStrLn $ Ghc.showSDocUnsafe $ Ghc.ppr (Ghc.idType <$> ids)
            dynFlags <- Ghc.getDynFlags
            extractionParams <-
              Map.traverseWithKey
                (\nm args -> do
                  ty <- Ghc.idType <$> Ghc.tcLookupId nm
                  let tySDoc = Ghc.ppr ty
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

            -- At this point all the information needed to modify the source
            -- should be at hand.
            -- Attempting to pretty print the AST as is would not work for various
            -- reasons, so firstly need to parse the AST from the source file.
            -- Then need to:
            -- 1) replace the decl that was extracted from with the corresponding
            -- decl from the TC AST. Problem: this necesitates turning a TC AST
            -- into a PS AST. Can make this easier with a surgical approach
            --  - Syb over the PS AST finding occurrences of EXTRACT
            --    - Because the src won't parse as is, need to first do a pass
            --      that converts EXTRACT@foo to EXTRACT foo
            --  - perhaps when doing that replace, can have the printed callsite
            --    from the TC AST on hand in a Map and simply replace it at that
            --    point. No because knowing the scope of the body is problematic.
            --  - after doing the replace, parse the AST
            --  - Syb over the PS AST and when EXTRACT app is encountered, lookup
            --    corresponding expr from the TC AST (will need to build a map)
            --    and graft it in, doing whatever conversions are necessary.
            --  - Will need to know what extracts are from which decls so that
            --    they can be inserted directly after that decl.
            --  - Insert the extract expr and synthesized sig into the PS AST.
            --    This is problematic because again a TC AST must be converted
            --    to a PS AST.
            --  - hold up, no such conversion should be necessary beyond the
            --    callsite one. Instead, use the App constructor from the PS
            --    AST to get the body expr.
            --  - Have the ordered list of arguments on hand. This can be used
            --    both for modifying the call site and adding the decl in the
            --    PS AST.
            --  - Therefore the input to src modification should be
            --    a map from name to arg names and inferred type
            pure gblEnv
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
    bndExtract (recFlag, bnds) =
      let (newBnds, newNames) = foldMap extract bnds
       in ((Ghc.Recursive, newBnds), newNames)

    extract :: Ghc.LHsBind Ghc.GhcRn -> ([Ghc.LHsBind Ghc.GhcRn], [(Ghc.Name, [Ghc.Name])])
    extract (Ghc.L loc bind) =
      let bindName = case bind of
            Ghc.FunBind _ n _ -> Just $ Ghc.unLoc n
            -- Ghc.PatBind -> _ TODO does this need to be handled?
            _ -> Nothing
          ((newBinds, newNames), updated) = Syb.everywhereM (Syb.mkM go) bind
          -- TODO use CPS writer
          go :: Ghc.HsExpr Ghc.GhcRn
             -> (([Ghc.HsBind Ghc.GhcRn], [(Ghc.Name, [Ghc.Name])]), Ghc.HsExpr Ghc.GhcRn)
          go = \case
                Ghc.HsLet _
                  (Ghc.HsValBinds _
                     (Ghc.XValBindsLR
                       (Ghc.NValBinds
                         [ ( recFlag
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
        , Just inputs <- Map.lookup (Ghc.occName n) extrDecls
        , let rdrName = Ghc.mkRdrUnqual . Ghc.mkVarOccFS $ Ghc.mkFastStringByteString realName
              arNames = Ghc.L Ghc.noSrcSpanA . Ghc.mkRdrUnqual <$> argNames inputs
              callsite = foldl'
                (\acc arg ->
                  Ghc.HsApp
                    Ghc.noExtField
                    (Ghc.noLocA acc)
                    (Ghc.noLocA $ Ghc.HsVar Ghc.noExtField arg)
                )
                (Ghc.HsVar Ghc.noExtField $ Ghc.noLocA rdrName)
                arNames
              grhss :: Ghc.GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
              grhss = Ghc.GRHSs Ghc.emptyComments
                        [Ghc.noLocA $ Ghc.GRHS Ghc.noSrcSpanA [] body]
                        (Ghc.EmptyLocalBinds Ghc.noExtField)
              newDecl = Ghc.L Ghc.noSrcSpanA $
                Ghc.ValD Ghc.noExtField $ Ghc.FunBind Ghc.noExtField (Ghc.noLocA rdrName) $
                  Ghc.MG Ghc.FromSource
                    (Ghc.L Ghc.noSrcSpanA
                      [Ghc.noLocA $ Ghc.Match
                        Ghc.noExtField
                        (Ghc.FunRhs (Ghc.noLocA rdrName) Ghc.Prefix Ghc.SrcLazy Ghc.noAnn)
                        (Ghc.noLocA $ Ghc.noLocA . Ghc.VarPat Ghc.noExtField <$> arNames)
                        grhss
                      ]
                    )
              mSig :: Maybe (Ghc.LHsDecl Ghc.GhcPs)
              mSig = do
                hsType <- extractedType inputs
                Just $ Ghc.L Ghc.noSrcSpanA $ Ghc.SigD Ghc.noExtField $
                  Ghc.TypeSig
                      (Ghc.AnnSig (Ghc.EpUniTok Ghc.noSpanAnchor Ghc.NormalSyntax) Nothing Nothing)
                      [Ghc.noLocA rdrName] $
                    Ghc.HsWC Ghc.noExtField $ Ghc.noLocA $
                      Ghc.HsSig Ghc.noExtField Ghc.mkHsOuterImplicit (Ghc.noLocA hsType)
        -> (maybe id (:) mSig [newDecl], callsite)
      x -> (mempty, x)
