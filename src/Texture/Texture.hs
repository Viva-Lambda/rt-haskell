-- module texture
module Texture.Texture where

import Vector

class Texture a where
    color :: a -> Double -> Double -> Vector -> Vector
