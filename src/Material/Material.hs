-- material module
module Material.Material where

import Vector
import Ray

data Material = LambMat Lambertian
              | MetalMat Metal
              | NoMat

type Color = Vector

-- lambertian material

data Lambertian = Lamb {lalbedo :: Color} deriving (Eq, Show)

-- metal material
data Metal = Met {malbedo :: Color, fuzz :: Double} deriving (Eq, Show)
