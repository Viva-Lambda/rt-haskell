-- module texture
module Texture.Texture where

import Math3D.Vector

class Texture a where
    color :: a -> Double -> Double -> Vector -> Vector
