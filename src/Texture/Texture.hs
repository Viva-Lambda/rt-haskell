-- module texture
module Texture.Texture where

import Math3D.Vector
import Color.ColorInterface

-- base types and enums
import Utility.BaseEnum

class Texture a where
    -- object -> u -> v -> hit point -> wave length -> Color information
    color :: a -> Double -> Double -> Vector -> WaveVal -> ColorRecord
