{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
module AutoExtract.GhcFacade
  ( module Ghc
  , diffLine
  , anchorD1
  ) where

import           GHC.Driver.Plugins as Ghc
import           GHC as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Data.StringBuffer as Ghc
import           GHC.Driver.Errors.Types as Ghc
import           GHC.Parser.Errors.Types as Ghc
import           GHC.Types.Error as Ghc
import           GHC.Driver.Pipeline.Phases as Ghc
import           GHC.Driver.Pipeline.Execute as Ghc
import           GHC.Types.SourceError as Ghc
import           GHC.Driver.Env as Ghc
import           GHC.Driver.Hooks as Ghc
import           GHC.Types.Name as Ghc
import           GHC.Types.Name.Reader as Ghc
import           GHC.Types.Basic as Ghc
import           GHC.Tc.Types as Ghc (TcGblEnv(..))
import           GHC.Types.Name.Set as Ghc
import           GHC.Data.FastString as Ghc
import           GHC.Tc.Utils.Env as Ghc
import           GHC.Core.TyCo.Ppr as Ghc
import           GHC.Utils.Error as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Driver.Ppr as Ghc
import           GHC.Types.Name.Ppr as Ghc
#if MIN_VERSION_ghc(9,8,0)
import           GHC.Driver.DynFlags as Ghc
#else
import           GHC.Driver.Session as Ghc
#endif

#if MIN_VERSION_ghc(9,10,0)
import qualified Language.Haskell.GHC.ExactPrint as EP
#endif

diffLine
  :: Int
  -> Int
#if MIN_VERSION_ghc(9,12,0)
  -> Ghc.NoAnn ann => Ghc.EpAnn ann
diffLine rowOffset colOffset =
  Ghc.EpAnn (Ghc.EpaDelta Ghc.noSrcSpan (Ghc.DifferentLine rowOffset colOffset) []) Ghc.noAnn Ghc.emptyComments
#elif MIN_VERSION_ghc(9,10,0)
  -> Ghc.NoAnn ann => Ghc.EpAnn ann
diffLine rowOffset colOffset =
  Ghc.EpAnn (Ghc.EpaDelta (Ghc.DifferentLine rowOffset colOffset) []) Ghc.noAnn Ghc.emptyComments
#elif MIN_VERSION_ghc(9,6,0)
  -> Monoid ann => Ghc.SrcAnn ann
diffLine rowOffset colOffset =
  Ghc.SrcSpanAnn
    (Ghc.EpAnn
      (Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor
        (if rowOffset == 0 then Ghc.SameLine colOffset else Ghc.DifferentLine rowOffset colOffset))
      )
      mempty
      Ghc.emptyComments
    )
    (Ghc.RealSrcSpan Ghc.placeholderRealSpan mempty)
#endif

anchorD1
#if MIN_VERSION_ghc(9,10,0)
  :: Ghc.NoAnn ann => Ghc.EpAnn ann
anchorD1 = Ghc.EpAnn EP.d1 Ghc.noAnn Ghc.emptyComments
#elif MIN_VERSION_ghc(9,6,0)
  :: Monoid ann => Ghc.SrcAnn ann

-- blarg
instance Monoid Ghc.NoEpAnns where
  mempty = Ghc.NoEpAnns

anchorD1 =
  Ghc.SrcSpanAnn
    (Ghc.EpAnn
      (Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.SameLine 1)))
      mempty
      Ghc.emptyComments
    )
    Ghc.generatedSrcSpan
#endif
