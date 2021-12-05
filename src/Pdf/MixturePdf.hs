{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- mixture pdf object
module Pdf.MixturePdf where

import Pdf.Pdf

-- math3d
import Math3D.Vector
import Math3D.CommonOps

-- utility etc
import Utility.Utils
import Utility.HelperTypes

-- 
import Random

-- other
import GHC.Float

-- mix n number of pdfs
data MixturePdf where
    MixPdf :: Pdf a => a -> a -> MixturePdf


instance Pdf MixturePdf where
    pvalue !mpdf gen !dir =
        case mpdf of
            (MixPdf a b) -> let (av, ga) = pvalue a gen dir
                                (bv, gb) = pvalue b ga dir
                                av_w = av * 0.5
                                bv_w = bv * 0.5
                            in (av_w + bv_w, gb)

    generate !mpdf g =
        case mpdf of
            (MixPdf a b) ->
                let (pv, g1) = randval g
                in if pv < 0.5
                   then generate a g1
                   else generate b g1
