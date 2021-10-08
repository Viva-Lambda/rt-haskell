-- module solid color
module Texture.SolidColor where

import Vector
import Texture.Texture

data SolidColor = SolidV Vector
                | SolidD Double Double Double

instance Texture SolidColor where
    color (SolidV a) _ _ _ = a
    color (SolidD a b c) _ _ _ = VList [a, b, c]
