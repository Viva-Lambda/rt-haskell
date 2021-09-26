-- material module
module Material.Material where

import Vector
import Ray

data Material = LambMat Lambertian
              | MetalMat Metal
              | DielMat Dielectric
              | NoMat

type Color = Vector

-- lambertian material

data Lambertian = Lamb {lalbedo :: Color} deriving (Eq, Show)

-- metal material
data Metal = Met {malbedo :: Color, fuzz :: Double} deriving (Eq, Show)

-- dielectric material
data Dielectric = Diel {refIndices :: [Double]}

schlickRef :: Double -> Double -> Double
schlickRef cosi ref_idx =
    let r0 = (1.0 - ref_idx) / (1.0 + ref_idx)
        r1 = r0 * r0
        pw = (1.0 - cosi) ** 5
    in r1 + (1.0 - r1) * pw
