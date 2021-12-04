{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- hittable pdf
module Pdf.HittablePdf where

import Pdf.Pdf

-- math
import Math3D.Vector

-- utility
import Utility.Utils

-- hittable
import Hittable.Hittable

data HittablePdf where
    HitPdf :: Hittable a => a -> Vector -> HittablePdf

instance Pdf HittablePdf where
    pvalue hpdf gen dir =
        case hpdf of
            (HitPdf a orig) ->  pdf_value a gen orig dir

    generate hpdf g =
        case hpdf of
            (HitPdf a orig) -> hrandom a g orig
