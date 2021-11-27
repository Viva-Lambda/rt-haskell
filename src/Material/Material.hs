{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- material module
module Material.Material where

import Math3D.Vector
import Math3D.Ray
import Texture.TextureObj

data Material = LambMat Lambertian
                | MetalMat Metal 
                | DielMat Dielectric
                | LightMat DiffuseLight
                | NoMat

type Color = Vector

-- data Lambertian = Lamb {lalbedo :: Color} deriving (Eq, Show)

-- lambertian material

data Lambertian = Lamb {lalbedo :: TextureObj}

{-
data Lambertian a where
    LambC :: Color -> Lambertian SolidColor
    LambT :: Texture a => a -> Lambertian a

-}

-- metal material
data Metal = Met {malbedo :: TextureObj, fuzz :: Double }

{-
data Metal a b where
    MetC :: Color -> Double -> Metal SolidColor Double
    MetT :: Texture a => a -> Double -> Metal a Double
-}

-- dielectric material
data Dielectric = Diel {refIndices :: [Double]}

schlickRef :: Double -> Double -> Double
schlickRef cosi ref_idx =
    let r0 = (1.0 - ref_idx) / (1.0 + ref_idx)
        r1 = r0 * r0
        pw = (1.0 - cosi) ** 5
    in r1 + (1.0 - r1) * pw

data DiffuseLight = DLight {emitTexture :: TextureObj}

