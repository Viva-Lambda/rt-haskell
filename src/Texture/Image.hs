{-# LANGUAGE BangPatterns #-}
-- image texture
module Texture.Image where

import Vector

data ImageT = Img {
        imgTWidth :: Int,
        imgTHeight :: Int,
        bytesPerScanline :: Int,
        bytesPerPixel :: Int,
        imgData :: [[Vector]]
    }
