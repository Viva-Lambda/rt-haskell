-- module texture
module Texture.Texture where

import Math3D.Vector
import Color.ColorInterface

class Texture a where
    color :: a -> Double -> Double -> Vector -> ColorInterface
