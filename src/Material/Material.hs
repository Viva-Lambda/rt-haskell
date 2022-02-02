{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- material module
module Material.Material where

import Math3D.Vector
import Math3D.Ray

import Texture.TextureObj
import Texture.Spectral

-- color related
import Color.ColorInterface

-- materials

data Material = LambMat Lambertian
                | MetalMat Metal 
                | DielMat Dielectric
                | LightMat DiffuseLight
                | IsotMat Isotropic
                | SpectralMat SpectralMaterial
                | NoMat

type Color = ColorRecord

-- data Lambertian = Lamb {lalbedo :: Color} deriving (Eq, Show)

-- lambertian material
-- data Lambertian = Lamb {lalbedo :: TextureObj}

data Lambertian where
    -- LambC :: Color -> Lambertian
    LambT :: TextureObj -> Lambertian


-- metal material
-- data Metal = Met {malbedo :: TextureObj, fuzz :: Double }

data Metal where
    -- MetC :: Color -> Double -> Metal
    MetT :: TextureObj -> Double -> Metal

-- dielectric material
data Dielectric where
    DielRefIndices :: [Double] -> Dielectric

schlickRef :: Double -> Double -> Double
schlickRef cosi ref_idx =
    let r0 = (1.0 - ref_idx) / (1.0 + ref_idx)
        r1 = r0 * r0
        pw = (1.0 - cosi) ** 5
    in r1 + (1.0 - r1) * pw

data DiffuseLight where 
    DLightEmitTextureCons :: TextureObj -> DiffuseLight
    -- DLightColorCons :: Color -> DiffuseLight
    

-- isotropic material

data Isotropic where
    IsotTexture :: TextureObj -> Isotropic
    -- IsotColor :: Color -> Isotropic

data SpectralMaterial where
    SpectralLamb :: SpectralTexture -> SpectralMaterial
    SpectralMetal :: SpectralTexture -> Double -> SpectralMaterial
    SpectralLight :: SpectralTexture -> SpectralMaterial
    SpectralIsotropic :: SpectralTexture -> SpectralMaterial
