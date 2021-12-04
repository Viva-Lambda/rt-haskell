{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- scatter record
module Material.ScatterRecord where

-- math
import Math3D.Ray
import Math3D.Vector

-- pdf handling
import Pdf.PdfObj
import Pdf.Pdf

data ScatterRecord where 
        ScatterRec :: Ray -> Bool -> Vector -> PdfObj -> ScatterRecord


mkSRecord :: Pdf a => Ray -> Bool -> Vector -> a -> ScatterRecord
mkSRecord r b v a = ScatterRec r b v (PdfCons a)

emptySRecord :: PdfObj -> Int -> ScatterRecord
emptySRecord pobj size = mkSRecord (zeroRay size) False (zeroV size) pobj

emptySRec :: PdfObj -> ScatterRecord
emptySRec pobj = emptySRecord pobj 3

specularRaySR :: ScatterRecord -> Ray
specularRaySR a = case a of
                    (ScatterRec r _ _ _) -> r

isSpecularSR :: ScatterRecord -> Bool
isSpecularSR a = case a of
                    (ScatterRec _ b _ _) -> b

attenuationSR :: ScatterRecord -> Vector
attenuationSR a = case a of
                    (ScatterRec _ _ b _) -> b

pdfPtrSR :: ScatterRecord -> PdfObj
pdfPtrSR a = case a of
                (ScatterRec _ _ _ b) -> b
