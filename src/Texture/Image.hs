{-# LANGUAGE BangPatterns #-}
-- image texture
module Texture.Image where

import Math3D.Vector
import Texture.Texture
import Utility.Utils

import GHC.Float hiding (clamp)
import Data.Word
import Data.Bitmap.Pure.Pixels
import Data.Bitmap.Base
import Codec.Image.STB
import qualified  Data.Map as DMap
import Data.List

type Row = Int
type Column = Int

type ImageData = [[Word8]]

data ImageT = ImgT {
        imgTWidth :: Int,
        imgTHeight :: Int,
        bytesPerScanline :: Int,
        bytesPerPixel :: Int,
        imgData :: ImageData
    }

instance Show ImageT where
    show (ImgT {imgTWidth = a, imgTHeight = b, 
                bytesPerScanline = c,
                bytesPerPixel = d, imgData = _}) = 
        let msg1 = "<Image Texture: " ++ (show a) ++ "x" ++ (show b)
            msg2 = "x" ++ (show d) ++ ">"
        in msg1 ++ msg2


debugImageT :: ImageT -> String
debugImageT img = 
    let imdata = imgData img
        msg1 = show img
        msg2 = show imdata
    in msg1 ++ " data: " ++ msg2

--
pixToDouble :: Word8 -> Double
pixToDouble p = fromRational $ toRational p

bitmapToImageT :: Image -> ImageT
bitmapToImageT !b =
    let (w, h) = bitmapSize b
        channels = bitmapNChannels b
        offsets = [(ww, hh) | hh <- [0..(h-1)], ww <- [0..(w-1)] ]
        readerFn oset = let p = unsafeReadPixel b oset
                        in p
        -- foldfn :: (a -> b -> a) :: ([] -> [a] -> [a])
        imdata = map (readerFn) offsets
        -- imindx = [0..((length imdata) - 1)]
        imgt = ImgT {imgTWidth = w, imgTHeight = h,
                     bytesPerPixel = channels,
                     bytesPerScanline = channels * w,
                     imgData = imdata}
    in imgt
    -- in error $ debugImageT imgt


instance Texture ImageT where
    color !imgt !u !v p =
        let (ImgT {imgTWidth = a, imgTHeight = b, bytesPerScanline = bps,
                     bytesPerPixel = bpp, imgData = imap}) = imgt
        in if null imap
           then VList [0.0, 1.0, 1.0]
           else let uu = clamp u 0.0 1.0
                    vv = 1.0 - (clamp v 0.0 1.0)
                    -- vv = clamp v 0.0 1.0
                    -- uu = 1.0 - (clamp u 0.0 1.0)
                    i_ = double2Int $ uu * (int2Double a)
                    j_ = double2Int $ vv * (int2Double b)
                    i = if i_ >= a
                        then a - 1 
                        else i_
                    j = if j_ >= b
                        then b - 1
                        else j_
                    cscale = 1.0 / 255.0
                    pix = let cs = [0..(bpp-1)]
                              coeff = (j * bps) + (i * bpp)
                              iis = [coeff + c | c <- cs]
                              -- vals = map pixToDouble [imap !! ii | ii <- iis]
                              vals = VList $ map pixToDouble (imap !! (j * a + i))
                          -- in VList [imap DMap.! ii | ii <- iis]
                          in vals
                    cval = multiplyS pix cscale
                in cval
                -- in error $ show imgt
