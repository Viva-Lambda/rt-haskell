{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- material module
module Material.Material where

import Vector
import Ray
import Texture.Texture
import Texture.SolidColor

data Material = LambMat Lambertian
                | MetalMat Metal 
                | DielMat Dielectric
                | NoMat
                deriving (Eq, Show)

type Color = Vector

data Lambertian = Lamb {lalbedo :: Color} deriving (Eq, Show)

{-
-- lambertian material

data Lambertian a where
    LambC :: Color -> Lambertian SolidColor
    LambT :: Texture a => a -> Lambertian a
-}

-- metal material
data Metal = Met {malbedo :: Color, fuzz :: Double } deriving (Eq, Show)

{-
data Metal a b where
    MetC :: Color -> Double -> Metal SolidColor Double
    MetT :: Texture a => a -> Double -> Metal a Double
-}

-- dielectric material
data Dielectric = Diel {refIndices :: [Double]} deriving (Eq, Show)

schlickRef :: Double -> Double -> Double
schlickRef cosi ref_idx =
    let r0 = (1.0 - ref_idx) / (1.0 + ref_idx)
        r1 = r0 * r0
        pw = (1.0 - cosi) ** 5
    in r1 + (1.0 - r1) * pw
