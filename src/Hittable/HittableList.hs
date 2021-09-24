-- hittable list module contains hittables and related
module Hittable.HittableList where

import Hittable.Hittable
import Hittable.Sphere

import Data.List
import Data.Function

data HittableObj = HitSphere Sphere

data HittableList = HList [HittableObj]

instance Hittable HittableObj where
    hit (HitSphere s) ry tmin tmax hrec = hit s ry tmin tmax hrec


instance Hittable HittableList where
    hit (HList (h:hs)) ry tmin tmax hrec =
        let hitobjs = hits tmax (h:hs) hrec -- [(hrec, Bool)]
        in if null hitobjs
           then (hrec, False)
           else minimumBy (compare `on` hrecDist) hitobjs
        where hits _ [] _ = []
              hits mx (t:ts) hr =
                let (nhrec, isHit) = hit t ry tmin mx hr
                    nhdist = dist nhrec
                in if isHit
                   then (nhrec, isHit) : hits nhdist ts nhrec
                   else hits mx ts hr
              hrecDist (a, _) = dist a
              hrecCompare (a, _) (b, _)
                           = compare (dist a) (dist b)
