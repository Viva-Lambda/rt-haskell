{-# LANGUAGE BangPatterns #-}
-- bounding volume hierarchy
module Hittable.Bvh where

import Hittable.Hittable
import Hittable.HittableList
import Hittable.Aabb

data Bvh = BNode HittableObj HittableObj Aabb
          | BLeaf
