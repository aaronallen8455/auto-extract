{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module AutoExtract.Expr
  ( mkExtractionSig
  , mkExtractionDecl
  , mkRewrittenLet
  , pattern ExtractionLetExpr
  , mkExtractionBind
  , nameToBS
  ) where

import qualified Data.ByteString as BS
import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified AutoExtract.GhcFacade as Ghc

mkExtractionSig :: Ghc.RdrName -> Ghc.HsType Ghc.GhcPs -> Ghc.LHsDecl Ghc.GhcPs
mkExtractionSig rdrName hsType =
  Ghc.L (Ghc.diffLine 2 0) $ Ghc.SigD Ghc.noExtField $
#if MIN_VERSION_ghc(9,12,0)
    Ghc.TypeSig
        (Ghc.AnnSig
          (Ghc.EpUniTok EP.d1 Ghc.NormalSyntax)
          Nothing
          Nothing
        )
        [Ghc.noLocA rdrName] $
      Ghc.HsWC Ghc.noExtField $ Ghc.L Ghc.anchorD1 $
        Ghc.HsSig Ghc.noExtField Ghc.mkHsOuterImplicit (Ghc.noLocA hsType)
#elif MIN_VERSION_ghc(9,10,0)
    Ghc.TypeSig
        (Ghc.AnnSig
          (Ghc.AddEpAnn Ghc.AnnDcolon EP.d1)
          []
        )
        [Ghc.noLocA rdrName] $
      Ghc.HsWC Ghc.noExtField $ Ghc.L Ghc.anchorD1 $
        Ghc.HsSig Ghc.noExtField Ghc.mkHsOuterImplicit (Ghc.noLocA hsType)
#else
    Ghc.TypeSig
        (Ghc.EpAnn
          (Ghc.Anchor Ghc.placeholderRealSpan EP.m0)
          (Ghc.AnnSig
            (Ghc.AddEpAnn Ghc.AnnDcolon EP.d1)
            []
          )
          Ghc.emptyComments
        )
        [Ghc.L (Ghc.diffLine 0 0) rdrName] $
      Ghc.HsWC Ghc.noExtField $ Ghc.L Ghc.anchorD1 $
        Ghc.HsSig Ghc.noExtField Ghc.mkHsOuterImplicit (Ghc.L (Ghc.diffLine 0 0) hsType)
#endif

--------------------------------------------------------------------------------

mkExtractionDecl
  :: Ghc.RdrName
  -> Ghc.LHsExpr Ghc.GhcPs
  -> [Ghc.GenLocated Ghc.SrcSpanAnnN Ghc.RdrName]
  -> Ghc.LHsDecl Ghc.GhcPs
mkExtractionDecl rdrName body arNames =
#if MIN_VERSION_ghc(9,12,0)
  Ghc.L (Ghc.diffLine 1 0) $
    Ghc.ValD Ghc.noExtField $ Ghc.FunBind Ghc.noExtField (Ghc.noLocA rdrName) $
      Ghc.MG Ghc.FromSource
        (Ghc.L Ghc.noSrcSpanA
          [Ghc.L Ghc.noAnn $ Ghc.Match
            Ghc.noExtField
            (Ghc.FunRhs (Ghc.noLocA rdrName) Ghc.Prefix Ghc.NoSrcStrict Ghc.noAnn)
            (Ghc.noLocA $ Ghc.L (Ghc.diffLine 0 1) . Ghc.VarPat Ghc.noExtField <$> arNames)
            grhss
          ]
        )
#elif MIN_VERSION_ghc(9,10,0)
  Ghc.L (Ghc.diffLine 1 1) $
    Ghc.ValD Ghc.noExtField $ Ghc.FunBind Ghc.noExtField (Ghc.L EP.noAnnSrcSpanDP0 rdrName) $
      Ghc.MG Ghc.FromSource
        (Ghc.L EP.noAnnSrcSpanDP0
          [Ghc.L Ghc.noAnn $ Ghc.Match
            []
            (Ghc.FunRhs (Ghc.L EP.noAnnSrcSpanDP0 rdrName) Ghc.Prefix Ghc.NoSrcStrict)
            (Ghc.L EP.noAnnSrcSpanDP0 . Ghc.VarPat Ghc.noExtField <$> arNames)
            grhss
          ]
        )
#else
  Ghc.L (Ghc.diffLine 1 0) $
    Ghc.ValD Ghc.noExtField $
      Ghc.FunBind Ghc.noExtField
        (Ghc.L (Ghc.diffLine 0 0) rdrName)
      $ Ghc.MG Ghc.FromSource
          (Ghc.L (Ghc.diffLine 0 0)
            [Ghc.L (Ghc.diffLine 0 0) $ Ghc.Match
              (Ghc.ann (Ghc.diffLine 0 0))
              (Ghc.FunRhs (Ghc.L (Ghc.diffLine 0 0) rdrName) Ghc.Prefix Ghc.NoSrcStrict)
              (Ghc.noLocA . Ghc.VarPat Ghc.noExtField <$> arNames)
              grhss
            ]
          )
#endif
  where
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
#elif MIN_VERSION_ghc(9,10,0)
    grhss = Ghc.GRHSs Ghc.emptyComments
              [Ghc.L Ghc.anchorD1 $ Ghc.GRHS
                (Ghc.EpAnn EP.d0
                  (Ghc.GrhsAnn Nothing (Ghc.AddEpAnn Ghc.AnnEqual EP.d0))
                  Ghc.emptyComments
                )
                []
                body
              ]
              (Ghc.EmptyLocalBinds Ghc.noExtField)
#else
    grhss = Ghc.GRHSs Ghc.emptyComments
              [Ghc.L Ghc.anchorD1 $ Ghc.GRHS
                (Ghc.EpAnn (Ghc.Anchor Ghc.placeholderRealSpan EP.m0)
                  (Ghc.GrhsAnn Nothing (Ghc.AddEpAnn Ghc.AnnEqual EP.d0))
                  Ghc.emptyComments
                )
                []
                body
              ]
              (Ghc.EmptyLocalBinds Ghc.noExtField)
#endif

--------------------------------------------------------------------------------

mkRewrittenLet
  :: Ghc.FastString
  -> Ghc.LHsExpr Ghc.GhcPs
  -> Ghc.HsExpr Ghc.GhcPs
mkRewrittenLet bnd body =
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
#elif MIN_VERSION_ghc(9,10,0)
      mg = Ghc.MG Ghc.FromSource
             (Ghc.L Ghc.noSrcSpanA
               [Ghc.noLocA $ Ghc.Match
                 []
                 (Ghc.FunRhs (Ghc.noLocA bndName) Ghc.Prefix Ghc.SrcLazy)
                 []
                 grhss
               ]
             )
#else
      mg = Ghc.MG Ghc.FromSource
             (Ghc.L Ghc.noSrcSpanA
               [Ghc.noLocA $ Ghc.Match
                 Ghc.noAnn
                 (Ghc.FunRhs (Ghc.noLocA bndName) Ghc.Prefix Ghc.SrcLazy)
                 []
                 grhss
               ]
             )
#endif
      grhss :: Ghc.GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
#if MIN_VERSION_ghc(9,10,0)
      grhss = Ghc.GRHSs Ghc.emptyComments
                [Ghc.noLocA $ Ghc.GRHS Ghc.noSrcSpanA [] body]
                (Ghc.EmptyLocalBinds Ghc.noExtField)
#else
      grhss = Ghc.GRHSs Ghc.emptyComments
                [Ghc.noLocA $ Ghc.GRHS Ghc.noAnn [] body]
                (Ghc.EmptyLocalBinds Ghc.noExtField)
#endif
      expr :: Ghc.HsExpr Ghc.GhcPs
#if MIN_VERSION_ghc(9,10,0)
      expr = Ghc.HsLet Ghc.noAnn
        (Ghc.HsValBinds Ghc.noSrcSpanA
           (Ghc.ValBinds Ghc.NoAnnSortKey
              [ Ghc.noLocA $ Ghc.FunBind Ghc.noExtField (Ghc.noLocA bndName) mg ]
              []
           )
        )
        (Ghc.noLocA $ Ghc.HsVar Ghc.noExtField (Ghc.noLocA bndName))
#else
      expr = Ghc.HsLet Ghc.noAnn Ghc.noHsTok
        (Ghc.HsValBinds Ghc.noAnn
           (Ghc.ValBinds Ghc.NoAnnSortKey
              [ Ghc.noLocA $ Ghc.FunBind Ghc.noExtField (Ghc.noLocA bndName) mg ]
              []
           )
        )
        Ghc.noHsTok
        (Ghc.noLocA $ Ghc.HsVar Ghc.noExtField (Ghc.noLocA bndName))
#endif
   in expr

--------------------------------------------------------------------------------

pattern ExtractionLetExpr
  :: Ghc.FreeVars
  -> Ghc.Name
  -> Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.HsExpr Ghc.GhcRn
pattern ExtractionLetExpr freeVars bndName grhss <-
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
    _
#elif MIN_VERSION_ghc(9,10,0)
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
#else
  Ghc.HsLet _ _
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
    _ _
#endif

--------------------------------------------------------------------------------

mkExtractionBind
  :: Ghc.FreeVars
  -> Ghc.Name
  -> [Ghc.Name]
  -> Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.HsBind Ghc.GhcRn
mkExtractionBind freeVars topLvlName args grhss =
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
#elif MIN_VERSION_ghc(9,10,0)
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
#else
  Ghc.FunBind freeVars (Ghc.noLocA topLvlName) $
    Ghc.MG Ghc.FromSource
      (Ghc.L Ghc.noSrcSpanA
        [Ghc.noLocA $ Ghc.Match
          Ghc.noAnn
          (Ghc.FunRhs (Ghc.noLocA topLvlName) Ghc.Prefix Ghc.SrcLazy)
          (Ghc.noLocA . Ghc.VarPat Ghc.noExtField . Ghc.noLocA <$> args)
          grhss
        ]
      )
#endif

nameToBS :: Ghc.HasOccName a => a -> BS.ByteString
nameToBS = Ghc.bytesFS . Ghc.occNameFS . Ghc.occName
