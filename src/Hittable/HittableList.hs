{-# LANGUAGE BangPatterns #-}
-- hittable list module contains hittables and related
module Hittable.HittableList where

import Hittable.Hittable
import Hittable.Sphere
import Hittable.MovingSphere
import Hittable.Aabb
import Hittable.HitRecord
import Material.Material

import Data.List
import Data.Function

data HittableObj = HitSphere !Sphere 
                 | MvHitSphere !MovingSphere
                    deriving (Show, Eq)

data HittableList = HList [HittableObj] deriving (Show, Eq)

instance Hittable HittableObj where
    hit !(HitSphere s) !ry !tmin !tmax !hrec = hit s ry tmin tmax hrec
    hit !(MvHitSphere s) !ry !tmin !tmax !hrec = hit s ry tmin tmax hrec
    boundingBox (MvHitSphere s) time0 time1 ab = boundingBox s time0 time1 ab
    boundingBox (HitSphere s) time0 time1 ab = boundingBox s time0 time1 ab


instance Hittable HittableList where
    hit !(HList (h:hs)) !ry !tmin !tmax !hrec =
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

    boundingBox !(HList hs) !time0 !time1 !ab =
        if null hs
        then (ab, False)
        else let tempBox = zeroAabb3
                 firstBox = True
             in bbox firstBox time0 time1 tempBox hs ab
        where bbox False t0 t1 tbox [] outBox = (outBox, True)
              bbox fbox t0 t1 tbox (htl:htls) outBox =
                let (sbox, hasBox) = boundingBox htl t0 t1 tbox
                in if hasBox == False
                   then (tbox, False)
                   else if fbox
                        then bbox False t0 t1 sbox htls sbox
                        else let obox = ssBox outBox sbox
                             in bbox False t0 t1 sbox htls obox

--
