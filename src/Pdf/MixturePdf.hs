{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- mixture pdf object
module Pdf.MixturePdf where

import Pdf.Pdf

-- math3d
import Math3D.Vector

-- utility etc
import Utility.Utils
import Utility.HelperTypes

-- 
import Random

-- other
import GHC.Float

-- mix n number of pdfs
data MixturePdf where
    MixPdf :: Pdf a => NonEmptyList a -> MixturePdf

instance Pdf MixturePdf where
    pvalue !mpdf gen !dir =
        case mpdf of
            (MixPdf mxs) -> let weight = 1.0 / (int2Double $ lengthNL mxs)
                                objs = nl2List mxs
                                fn acc pobj = let RandResult (pval, g) = acc
                                                  res = pvalue pobj g dir
                                                  res2 = rfmap (+ pval) res
                                              in rfmap (* weight) res2
                            in foldl fn (RandResult (0.0, gen)) objs

    generate !mpdf g = 
        case mpdf of
            (MixPdf mxs) ->
                let upper = lengthNL $! mxs
                    RandResult (index, g2) = randomInt g (0, upper)
                in generate (getNL mxs index) g2
