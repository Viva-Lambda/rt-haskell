{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- type that implements pdf
module Pdf.PdfObj where

import Pdf.Pdf
import Math3D.Vector

data NoPdf = NPdf {npdfVal :: Bool}

data PdfObj where
    PdfCons :: Pdf a => a -> PdfObj

instance Pdf PdfObj where

    pvalue a g v =
        case a of
            PdfCons b -> pvalue b g v

    generate a g =
        case a of
            PdfCons b -> generate b g

instance Pdf NoPdf where
    pvalue _ g _ = (0.0, g)
    generate _ g = randomVec (0.0, 1.0) g

emptyPdfObj :: PdfObj
emptyPdfObj = PdfCons $ NPdf {npdfVal = False}
