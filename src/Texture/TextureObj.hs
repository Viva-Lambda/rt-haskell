-- module grouping texture adts
module Texture.TextureObj where

import Vector
import Texture.SolidColor
import Texture.Checker
import Texture.Texture

data TextureObj = SolidTexture SolidColor
                | CheckerTexture Checker

instance Texture TextureObj where
    color (SolidTexture a) hu hv hp = color a hu hv hp
    color (CheckerTexture a) hu hv hp = color a hu hv hp
