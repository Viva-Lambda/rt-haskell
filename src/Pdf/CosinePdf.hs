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
import Random

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
               in RandResult (rval, gen)
            CosNormalPdf v -> pvalue (CosPdf $ fromW2Onb v) gen dir

    generate !a g =
        case a of 
           CosPdf onb -> let res = randomCosineDir g
                         in rfmap (localVec onb) res

           CosNormalPdf v -> generate (CosPdf $! fromW2Onb v) g
