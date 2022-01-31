-- module texture
module Texture.Texture where

import Math3D.Vector
import Color.ColorInterface

class Texture a where
    -- object -> u -> v -> hit point -> wave length -> Color information
    color :: a -> Double -> Double -> Vector -> Word -> ColorRecord
