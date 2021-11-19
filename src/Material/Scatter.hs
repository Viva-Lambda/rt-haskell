{-# LANGUAGE BangPatterns #-}
-- material module
module Material.Scatter where

import Math3D.Ray
import Math3D.Vector
-- import Math3D.CommonOps
import System.Random
import Random
import Hittable.HitRecord
import Material.Material
import Texture.Texture


type Attenuation = Vector
type ScatteredRay = Ray
type SOutput = (Attenuation, ScatteredRay, Bool)

class Scatterer a where
    scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> (g, Attenuation, ScatteredRay, Bool)
    emitted :: a -> Double -> Double -> Vector -> Vector

-- scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> Attenuation -> 
-- ScatteredRay -> (Attenuation, ScatteredRay, Bool)
instance Scatterer Material where
    scatter gen NoMat r h = (gen, zeroV3, r, False)
    scatter gen (LambMat la) r h  = scatter gen la r h
    scatter gen (MetalMat m) r h  = scatter gen m r h
    scatter gen (DielMat m) r h  = scatter gen m r h
    scatter gen (LightMat m) r h = scatter gen m r h
    emitted (LightMat m) u v p = emitted m u v p
    emitted (NoMat) u v p = zeroV3
    emitted (LambMat _) u v p = zeroV3
    emitted (MetalMat _) u v p = zeroV3
    emitted (DielMat _) u v p = zeroV3


instance Scatterer Lambertian where
    emitted _ _ _ _ = zeroV3
    scatter !gen !(Lamb {lalbedo = a}) !inray !hrec =
        let recp = point hrec
            recn = pnormal hrec
            (uvec, g) = randomUnitVector gen
            sdir = add recn uvec
            hu = hUV_u hrec
            hv = hUV_v hrec
        in if nearZeroVec sdir
           then (g, color a hu hv recp, 
                Rd {origin = recp,
                          direction = recn,
                          rtime = rtime inray}, True)
           else (g, color a hu hv recp, 
                 Rd {origin = recp, 
                           direction = sdir,
                           rtime = rtime inray}, True)

instance Scatterer Metal where
    emitted _ _ _ _ = zeroV3
    scatter !gen !(Met {malbedo = a, fuzz = b}) !inray !hrec =
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

instance Scatterer Dielectric where
    emitted _ _ _ _ = zeroV3
    scatter !gen !(Diel {refIndices = rs}) !inray !hrec =
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
    emitted (DLight{emitTexture = a}) u v p = color a u v p
