{-# LANGUAGE BangPatterns #-}
-- module grouping texture adts
module Texture.TextureObj where

import Vector
import Texture.SolidColor
import Texture.Checker
import Texture.Noise
import Texture.Texture
import Texture.Image

data TextureObj = SolidTexture SolidColor
                | CheckerTexture Checker
                | NoiseTexture PerlinNoise
                | ImageTexture ImageT

instance Texture TextureObj where
    color !(SolidTexture a) hu hv hp = color a hu hv hp
    color !(CheckerTexture a) hu hv hp = color a hu hv hp
    color !(NoiseTexture a) hu hv hp = color a hu hv hp
    color !(ImageTexture a) hu hv hp = color a hu hv hp
