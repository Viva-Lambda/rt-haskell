-- material module
module Material.Scatter where

import Ray
import Vector
import System.Random
import Random
import Hittable.HitRecord
import Material.Material


type Attenuation = Vector
type ScatteredRay = Ray
type SOutput = (Attenuation, ScatteredRay, Bool)

class Scatterer a where
    scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> (g, Attenuation, ScatteredRay, Bool)

-- scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> Attenuation -> 
-- ScatteredRay -> (Attenuation, ScatteredRay, Bool)
instance Scatterer Material where
    scatter gen NoMat r h = (gen, zeroV3, r, False)
    scatter gen (LambMat la) r h  = scatter gen la r h
    scatter gen (MetalMat m) r h  = scatter gen m r h
    scatter gen (DielMat m) r h  = scatter gen m r h


instance Scatterer Lambertian where
    scatter gen (Lamb {lalbedo = a}) inray hrec =
        let recp = point hrec
            recn = pnormal hrec
            (uvec, g) = randomUnitVector gen
            sdir = add recn uvec
        in if nearZeroVec sdir
           then (g, a,Rd {origin = recp, direction = recn}, True)
           else (g, a, Rd {origin = recp, direction = sdir}, True)

instance Scatterer Metal where
    scatter gen (Met {malbedo = a, fuzz = b}) inray hrec =
        let recp = point hrec
            recn = pnormal hrec
            indir = toUnit $! direction inray
            refdir = reflect indir recn
            (uvec, g) = randomUnitSphere gen
            rdir = add refdir (multiplyS uvec b)
        in (g, a, Rd {origin = recp, direction = rdir}, (dot rdir recn) > 0)

instance Scatterer Dielectric where
    scatter gen (Diel {refIndices = rs}) inray hrec =
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
            (rval, g) = rand gen
            schlickVal = schlickRef costheta refratio
            rdir = if canNotRefract || (schlickVal > rval)
                   then reflect udir (pnormal hrec)
                   else refract udir (pnormal hrec) refratio
            outray = Rd {origin = point hrec, direction = rdir}
        in (g, atten, outray, True)
