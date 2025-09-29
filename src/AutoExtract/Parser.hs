{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoExtract.Parser
  ( Extraction(..)
  , ExtractedDecls
  , modifyParsedDecls
  , pattern ExtractPat
  ) where

import qualified Control.Monad.Trans.Writer.CPS as W
#if !MIN_VERSION_ghc(9,10,0)
import           Data.Foldable -- foldl'
#endif
import qualified Data.ByteString as BS
import qualified Data.Generics as Syb
import qualified Data.Map.Strict as Map

import           AutoExtract.Expr
import qualified AutoExtract.GhcFacade as Ghc

type ExtractedDecls = Map.Map BS.ByteString Extraction

data Extraction = Extraction
  { argNames :: [Ghc.OccName]
  , extractedType :: Maybe (Ghc.HsType Ghc.GhcPs)
  }


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

    decls :: Ghc.FastString -> Ghc.LHsExpr Ghc.GhcPs -> Extraction -> W.Writer [Ghc.LHsDecl Ghc.GhcPs] (Ghc.HsExpr Ghc.GhcPs)
    decls bnd body inputs =
      let rdrName = Ghc.mkRdrUnqual $ Ghc.mkVarOccFS bnd
          arNames = Ghc.L Ghc.anchorD1 . Ghc.mkRdrUnqual <$> argNames inputs
          callsite = foldl'
            (\acc arg ->
#if MIN_VERSION_ghc(9,10,0)
              Ghc.HsApp
                Ghc.noExtField
                (Ghc.noLocA acc)
                (Ghc.L Ghc.noAnn $ Ghc.HsVar Ghc.noExtField arg)
#else
              Ghc.HsApp
                Ghc.noComments
                (Ghc.noLocA acc)
                (Ghc.L Ghc.noSrcSpanA $ Ghc.HsVar Ghc.noExtField arg)
#endif
            )
            (Ghc.HsVar Ghc.noExtField $ Ghc.noLocA rdrName)
            arNames
          newDecl :: Ghc.LHsDecl Ghc.GhcPs
          newDecl = mkExtractionDecl rdrName body arNames
          mSig :: Maybe (Ghc.LHsDecl Ghc.GhcPs)
          mSig = do
            hsType <- extractedType inputs
            Just $ mkExtractionSig rdrName hsType
       in W.writer (callsite, maybe id (:) mSig [newDecl])

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
#if MIN_VERSION_ghc(9,10,0)
removeParens (Ghc.L l (Ghc.HsPar _ (Ghc.L _ x))) = Ghc.L l (doIndentCorrect x)
#else
removeParens (Ghc.L l (Ghc.HsPar _ _ (Ghc.L _ x) _)) = Ghc.L l x
#endif
removeParens x = x

-- | exact-print 9.10 has indentation issues. This corrects for do blocks.
#if MIN_VERSION_ghc(9,12,0)
doIndentCorrect :: Ghc.HsExpr Ghc.GhcPs -> Ghc.HsExpr Ghc.GhcPs
doIndentCorrect x = x
#elif MIN_VERSION_ghc(9,10,0)
doIndentCorrect :: Ghc.HsExpr Ghc.GhcPs -> Ghc.HsExpr Ghc.GhcPs
doIndentCorrect (Ghc.HsDo ann t (Ghc.L l2 s)) =
  let addCol = \case
        Ghc.EpaDelta (Ghc.DifferentLine r c) a ->
          Ghc.EpaDelta (Ghc.DifferentLine r (c + 1)) a
        x -> x
   in Ghc.HsDo ann t (Ghc.L l2 {Ghc.entry = addCol (Ghc.entry l2)} s)
doIndentCorrect x = x
#else
#endif
