{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
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
import           Data.Maybe
import           Data.String
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
import qualified Language.Haskell.GHC.ExactPrint.Utils as EP

import           AutoExtract.Expr
import qualified AutoExtract.GhcFacade as Ghc
import           AutoExtract.Parser (Extraction(..), ExtractedDecls, modifyParsedDecls, pattern ExtractPat)
import           AutoExtract.Renamer (performExtractions)

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.driverPlugin = \_opts env -> updateHscEnv env
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
          unitEnv = Ghc.hsc_unit_env hscEnv
          namePprCtx = Ghc.mkNamePprCtx (Ghc.initPromotionTickContext dynFlags) unitEnv (Ghc.tcg_rdr_env gblEnv)
      extractionParams <- Map.mapKeys nameToBS <$>
        Map.traverseWithKey
          (\nm args -> do
            ty <- Ghc.idType <$> Ghc.tcLookupId nm
            -- Converting Type to HsType would be tedious so instead we
            -- pretty print the Type and run it through the type parser.
            let tySDoc = Ghc.pprSigmaType ty
                tyStr = Ghc.showSDocForUser
                          dynFlags { Ghc.pprCols = 5000 }
                          (Ghc.hsc_units hscEnv)
                          namePprCtx
                          tySDoc
                mHsTy = either (const Nothing) (Just . EP.makeDeltaAst)
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
      ExtractPat bnd body -> mkRewrittenLet bnd body
      x -> x

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
      updatedDecls = modifyParsedDecls extractedDecls (runTransform $ EP.hsDecls ast)
      updatedMod = runTransform $ EP.replaceDecls ast updatedDecls
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

#if MIN_VERSION_ghc(9,10,0)
runTransform :: a -> a
runTransform = id
#else
runTransform :: EP.Transform a -> a
runTransform t = case EP.runTransform t of
                   (a, _, _) -> a
#endif

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
