-- module for hittable type
module Hittable.Hittable where

import Math3D.Ray
import Hittable.HitRecord
import Hittable.Aabb
import System.Random

class Hittable a where
    hit :: RandomGen g => a -> g -> Ray -> Double -> Double -> HitRecord -> (HitRecord, Bool, g)
    boundingBox :: a -> Double -> Double -> Aabb -> (Aabb, Bool)
