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
    scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> Attenuation -> ScatteredRay -> (g, Attenuation, ScatteredRay, Bool)

-- scatter :: RandomGen g => g -> a -> Ray -> HitRecord -> Attenuation -> 
-- ScatteredRay -> (Attenuation, ScatteredRay, Bool)
instance Scatterer Material where
    scatter gen NoMat r h a s = (gen, a, s, False)
    scatter gen (LambMat la) r h a s = scatter gen la r h a s
    scatter gen (MetalMat m) r h a s = scatter gen m r h a s


instance Scatterer Lambertian where
    scatter gen (Lamb {lalbedo = a}) inray hrec att outray =
        let recp = point hrec
            recn = pnormal hrec
            (uvec, g) = randomUnitVector gen
            sdir = add recn uvec
        in if nearZeroVec sdir
           then (g, a,Rd {origin = recp, direction = recn}, True)
           else (g, a, Rd {origin = recp, direction = sdir}, True)

instance Scatterer Metal where
    scatter gen (Met {malbedo = a}) inray hrec att outray =
        let recp = point hrec
            recn = pnormal hrec
            indir = toUnit $! direction inray
            refdir = reflect indir recn
        in (gen, a, Rd {origin = recp, direction = refdir}, (dot refdir recn) > 0)

