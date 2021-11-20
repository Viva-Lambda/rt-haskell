{-# LANGUAGE BangPatterns #-}
-- hittable list module contains hittables and related
module Hittable.HittableList where

import Hittable.Hittable
import Hittable.HittableObj
import Hittable.Sphere
import Hittable.MovingSphere
import Hittable.Aabb
import Hittable.HitRecord
import Material.Material


import Utility.HelperTypes

import Data.List
import Data.Function


data HittableList = HList {objects :: NonEmptyList HittableObj}


instance Hittable HittableList where
    {-# INLINE hit #-}
    hit !hobj !ry !tmin !tmax !hrec =
        let hs = toList $ objects hobj
            hitobjs = hits tmax hs hrec -- [(hrec, Bool)]
        in if null hitobjs
           then (hrec, False)
           else minimumBy (compare `on` hrecDist) hitobjs
        where hits _ [] _ = []
              hits mx (t:ts) hr =
                let (nhrec, isHit) = hit t ry tmin mx hr
                    nhdist = hdist nhrec
                in if isHit
                   then (nhrec, isHit) : hits nhdist ts nhrec
                   else hits mx ts hr
              hrecDist (a, _) = hdist a

    boundingBox !hobjs !time0 !time1 !ab =
        let tempBox = zeroAabb3
            hs = toList $! objects hobjs
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
