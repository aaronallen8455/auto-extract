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
import           Data.Monoid (Any(..))
import qualified AutoExtract.GhcFacade as Ghc

import           Debug.Trace

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.driverPlugin = \_opts env -> updateHscEnv env
  }

updateHscEnv :: Ghc.HscEnv -> IO Ghc.HscEnv
updateHscEnv hscEnv = do
  hasExtractRef <- newIORef False
  extractedNamesRef <- newIORef Ghc.emptyNameSet
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
              liftIO $ modifyIORef extractedNamesRef (Ghc.unionNameSet newNames)
              liftIO $ putStrLn $ Ghc.showSDocUnsafe $ Ghc.ppr newGrp
              pure (gblEnv, newGrp)
            else pure (gblEnv, grp)
        , Ghc.typeCheckResultAction = \_ _ gblEnv -> do
            extractedNames <- liftIO $ readIORef extractedNamesRef
--             liftIO $ putStrLn $ Ghc.showSDocUnsafe $ Ghc.ppr extractedNames
--             liftIO $ putStrLn $ Ghc.showSDocUnsafe $ Ghc.ppr $ Ghc.tcg_binds gblEnv
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
            let bndName = Ghc.mkVarUnqual $ bnd <> "_EXTRACT" -- TODO get rid of suffix
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

performExtractions :: Ghc.TcGblEnv -> Ghc.HsGroup Ghc.GhcRn -> (Ghc.NameSet, Ghc.HsGroup Ghc.GhcRn)
performExtractions gblEnv grp =
  case Ghc.hs_valds grp of
    Ghc.XValBindsLR (Ghc.NValBinds bndTups sigs)
      | let extracted = bndExtract <$> bndTups ->
        ( foldMap snd extracted
        , grp
          { Ghc.hs_valds = Ghc.XValBindsLR (Ghc.NValBinds (fst <$> extracted) sigs) }
        )
    _ -> (Ghc.emptyNameSet, grp)
  where
    bndExtract :: (Ghc.RecFlag, Ghc.LHsBinds Ghc.GhcRn) -> ((Ghc.RecFlag, Ghc.LHsBinds Ghc.GhcRn), Ghc.NameSet)
    bndExtract (recFlag, bnds) =
      let (Any isRec, newBnds, newNames) = foldMap extract bnds
       in ((Ghc.Recursive, newBnds), newNames)

    extract :: Ghc.LHsBind Ghc.GhcRn -> (Any, [Ghc.LHsBind Ghc.GhcRn], Ghc.NameSet)
    extract (Ghc.L loc bind) =
      let bindName = case bind of
            Ghc.FunBind _ n _ -> Just $ Ghc.unLoc n
            -- Ghc.PatBind -> _ TODO does this need to be handled?
            _ -> Nothing
          ((isRec, newBinds, newNames), updated) = Syb.everywhereM (Syb.mkM go) bind
          -- TODO use CPS writer
          go :: Ghc.HsExpr Ghc.GhcRn -> ((Any, [Ghc.HsBind Ghc.GhcRn], Ghc.NameSet), Ghc.HsExpr Ghc.GhcRn)
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
                    -> let rec = Any $ maybe False (`Ghc.elemNameSet` freeVars) bindName -- TODO remove?
                           args = List.sort
                                . Ghc.nonDetEltsUniqSet
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
                        in ((rec, [newBind], Ghc.unitNameSet topLvlName), newExpr)

                x -> pure x

       in (isRec, (Ghc.noLocA <$> newBinds) ++ [Ghc.L loc updated], newNames)

    topLevelNames :: Ghc.NameSet
    topLevelNames = foldMap (fold . fst) $ Ghc.tcg_dus gblEnv
