{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- cosine pdf type
module Pdf.CosinePdf where

import Pdf.Pdf

-- math
import Math3D.Onb
import Math3D.Vector

-- utility
import Utility.Utils

data CosinePdf where
    CosPdf :: OrthoNormalBase -> CosinePdf
    CosNormalPdf :: Vector -> CosinePdf

instance Pdf CosinePdf where
    pvalue !a gen !dir =
        case a of
            CosPdf onb ->
               let cval = dot (toUnit dir) (wBasis onb)
                   rval = if cval <= 0.0
                          then 0.0
                          else cval / m_pi
               in (rval, gen)
            CosNormalPdf v -> pvalue (CosPdf $ fromW2Onb v) gen dir

    generate !a g =
        case a of 
           CosPdf onb -> let (rv, g1) = randomCosineDir g
                         in (localVec onb rv, g1)

           CosNormalPdf v -> generate (CosPdf $! fromW2Onb v) g
