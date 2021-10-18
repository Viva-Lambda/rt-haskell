{-# LANGUAGE BangPatterns #-}
-- image texture
module Texture.Image where

import Vector
import Texture.Texture
import Utility.Utils

import GHC.Float hiding (clamp)
import Data.Word
import Codec.Image.STB
import Data.Bitmap.Pure.Pixels


data ImageT = ImgT {
        imgTWidth :: Int,
        imgTHeight :: Int,
        bytesPerScanline :: Int,
        bytesPerPixel :: Int,
        imgData :: [Double]
    }

--
pixToVec :: PixelComponent t => t -> Double
pixToVec p = fromRational . toRational p

bitmapToImageT :: Bitmap Image -> ImageT
bitmapToImageT b =
    let (w, h) = bitmapSize b
        channels = bitmapNChannels b
        offsets = [(ww, hh) | ww <- w, hh <- h]
        readerFn = case channels of
                        1 -> unsafeReadPixel1
                        2 -> unsafeReadPixel2
                        3 -> unsafeReadPixel3
                        4 -> unsafeReadPixel4
                        _ -> error "unknown channel number" 
        imdata = readerFn $ 


instance Texture ImageT where
    color (ImgT {imgTWidth = a, imgTHeight = b, bytesPerScanline = _,
                 bytesPerPixel = _, imgData = vs})
          u v p = if vs == [[]]
                  then VList [0.0, 1.0, 1.0]
                  else let uu = clamp u 0.0 1.0
                           vv = 1.0 - (clamp v 0.0 1.0)
                           i_ = double2Int $ uu * (int2Double a)
                           j_ = double2Int $ vv * (int2Double b)
                           i = if i_ >= a
                               then a - 1 
                               else i_
                           j = if j >= b
                               then b - 1
                               else j_
                           cscale = 1.0 / 255.0
                           pix = (vs !! i) !! j
                           cval = multiplyS pix cscale
                       in cval
