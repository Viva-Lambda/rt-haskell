-- module for hittable type
module Hittable.Hittable where

import Ray
import Hittable.HitRecord

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> HitRecord -> (HitRecord, Bool)

