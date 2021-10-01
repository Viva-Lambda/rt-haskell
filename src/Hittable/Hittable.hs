-- module for hittable type
module Hittable.Hittable where

import Ray
import Hittable.HitRecord
import Hittable.Aabb

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> HitRecord -> (HitRecord, Bool)
    boundingBox :: a -> Double -> Double -> Aabb -> (Aabb, Bool)

