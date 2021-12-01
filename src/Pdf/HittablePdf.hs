{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- hittable pdf
module Pdf.HittablePdf where

import Pdf.Pdf

-- math
import Math3D.Onb
import Math3D.Vector

-- utility
import Utility.Utils

-- hittable
import Hittable.Hittable

data HittablePdf where
    HitPdf :: Hittable a => a -> Vector -> HittablePdf
