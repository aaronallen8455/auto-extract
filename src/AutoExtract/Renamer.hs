{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoExtract.Renamer
  ( performExtractions
  ) where

import qualified Control.Monad.Trans.Writer.CPS as W
import qualified Data.ByteString as BS
import           Data.Foldable
import qualified Data.Generics as Syb
import qualified GHC.IsList as IsList

import           AutoExtract.Expr
import qualified AutoExtract.GhcFacade as Ghc

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
            ExtractionLetExpr freeVars bndName grhss
              | Just occNameBS <- BS.stripSuffix "_EXTRACT" $ nameToBS bndName
              -> let args = Ghc.nameSetElemsStable
                          . Ghc.delFVs newDeclNames
                          $ freeVars `Ghc.minusNameSet` topLevelNames
                     topLvlName = Ghc.tidyNameOcc bndName (Ghc.mkVarOccFS $ Ghc.mkFastStringByteString occNameBS)
                     newBind = mkExtractionBind freeVars topLvlName args grhss
                     newExpr =
                       foldl'
                         (\acc arg ->
                           Ghc.HsApp
#if MIN_VERSION_ghc(9,10,0)
                             Ghc.noExtField
#else
                             Ghc.noComments
#endif
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
#if MIN_VERSION_ghc(9,10,0)
                Ghc.PatBind fvs a b c ->
                  pure $ Ghc.PatBind (Ghc.extendNameSetList fvs newDeclNames) a b c
#else
                Ghc.PatBind fvs a b ->
                  pure $ Ghc.PatBind (Ghc.extendNameSetList fvs newDeclNames) a b
#endif
                x -> pure x

       in (Ghc.L loc updated : (Ghc.noLocA <$> reverse newBinds), newNames)

    topLevelNames :: Ghc.NameSet
    topLevelNames = foldMap (fold . fst) $ Ghc.tcg_dus gblEnv

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
