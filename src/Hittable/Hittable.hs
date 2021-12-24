-- module for hittable type
module Hittable.Hittable where

-- math
import Math3D.Ray
import Math3D.Vector

import System.Random
import Random

-- hittable
import Hittable.HitRecord
import Hittable.Aabb

class Hittable a where
    hit :: RandomGen g => a -> g -> Ray -> Double -> Double -> HitRecord -> (HitRecord, Bool, g)
    boundingBox :: a -> Double -> Double -> Aabb -> (Aabb, Bool)

    pdf_value :: RandomGen g => a -> g -> Vector -> Vector -> RandomResult Double g
    hrandom :: RandomGen g => a -> g -> Vector -> RandomResult Vector g
