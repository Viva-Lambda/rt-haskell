{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- material module
module Material.Scatter where

import Math3D.Ray
import Math3D.Vector
import Math3D.CommonOps
import System.Random
import Random
import Hittable.HitRecord
import Material.Material

-- textures
import Texture.Texture
import Texture.TextureObj
import Texture.SolidColor


type Attenuation = Vector
type ScatteredRay = Ray
type SOutput = (Attenuation, ScatteredRay, Bool)

class Scatterer a where
    scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> (g, Attenuation, ScatteredRay, Bool)
    emitted :: a -> Double -> Double -> Vector -> Vector
    scattering_pdf :: a -> Ray -> HitRecord -> Ray -> Double


-- scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> Attenuation -> 
-- ScatteredRay -> (Attenuation, ScatteredRay, Bool)

instance Scatterer Material where
    scatter gen a r h = 
        case a of
            NoMat -> (gen, zeroV3, r, False)
            (LambMat la) -> scatter gen la r h
            (MetalMat m) -> scatter gen m r h
            (DielMat m) -> scatter gen m r h
            (LightMat m) -> scatter gen m r h
            (IsotMat m) -> scatter gen m r h

    emitted a u v p =
        case a of
            (LightMat m) -> emitted m u v p
            _ -> zeroV3

    scattering_pdf a r hrec sr = 0.0


instance Scatterer Lambertian where
    emitted _ _ _ _ = zeroV3

    scatter !gen !a !inray !hrec =
        case a of
            LambT t ->
                let recp = point hrec
                    recn = pnormal hrec
                    (uvec, g) = randomUnitVector gen
                    sdir = add recn uvec
                    hu = hUV_u hrec
                    hv = hUV_v hrec
                in if nearZeroVec sdir
                   then (g, color t hu hv recp, 
                        Rd {origin = recp,
                                  direction = recn,
                                  rtime = rtime inray}, True)
                   else (g, color t hu hv recp, 
                         Rd {origin = recp, 
                                   direction = sdir,
                                   rtime = rtime inray}, True)
            -- Color
            LambC c -> let lambtxt = TextureCons $ SolidV c
                       in scatter gen (LambT lambtxt) inray hrec


instance Scatterer Metal where
    emitted _ _ _ _ = zeroV3
    scatter !gen !c !inray !hrec =
        case c of
            (MetT a b) -> 
                let recp = point hrec
                    recn = pnormal hrec
                    hu = hUV_u hrec
                    hv = hUV_v hrec
                    indir = toUnit $! direction inray
                    refdir = reflect indir recn
                    (uvec, g) = randomUnitSphere gen
                    rdir = add refdir (multiplyS uvec b)
                in (g, color a hu hv recp, Rd {origin = recp, 
                              direction = rdir, 
                              rtime = rtime inray}, (dot rdir recn) > 0)
            (MetC a b) -> let mt = TextureCons $ SolidV a
                          in scatter gen (MetT mt b) inray hrec


instance Scatterer Dielectric where
    emitted _ _ _ _ = zeroV3
    scatter !gen !a !inray !hrec =
        case a of
            (DielRefIndices rs) ->
                let atten = VList [1.0, 1.0, 1.0]
                    -- can change with respect to wavelength
                    ir = head rs
                    refratio = if isFront hrec
                               then 1.0 / ir
                               else ir
                    udir = toUnit $! direction inray
                    costheta = min (dot (multiplyS udir (-1.0)) (pnormal hrec)) 1.0
                    sintheta = sqrt (1.0 - costheta * costheta)
                    canNotRefract = refratio * sintheta > 1.0
                    (rval, g) = randval gen
                    schlickVal = schlickRef costheta refratio
                    rdir = if canNotRefract || (schlickVal > rval)
                           then reflect udir (pnormal hrec)
                           else refract udir (pnormal hrec) refratio
                    outray = Rd {origin = point hrec,
                                 direction = rdir,
                                 rtime = rtime inray
                                 }
                in (g, atten, outray, True)

instance Scatterer DiffuseLight where
    scatter !gen !a !inray !hrec = (gen, zeroV3, inray, False)
    emitted b u v p = 
        case b of
            DLightEmitTextureCons a -> color a u v p
            DLightColorCons a -> let b = TextureCons $ SolidV a
                                 in color b u v p

instance Scatterer Isotropic where
    emitted _ _ _ _ = zeroV3
    scatter !gen !b !inray !hrec =
        case b of
            IsotTexture a ->
                let (uvec, g) = randomUnitSphere gen
                    recp = point hrec
                    hu = hUV_u hrec
                    hv = hUV_v hrec
                    outray = Rd {origin = recp, 
                                 direction = uvec, 
                                 rtime = rtime inray}
                    atten = color a hu hv recp
                in (g, atten, outray, True)

            IsotColor c -> let mt = TextureCons $ SolidV c
                           in scatter gen (IsotTexture mt) inray hrec


