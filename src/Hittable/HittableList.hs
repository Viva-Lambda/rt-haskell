{-# LANGUAGE BangPatterns #-}
-- hittable list module contains hittables and related
module Hittable.HittableList where

-- hittable
import Hittable.Hittable
import Hittable.HittableObj
import Hittable.Sphere
import Hittable.MovingSphere
import Hittable.Aabb
import Hittable.HitRecord

-- material
import Material.Material

-- utility etc
import Utility.HelperTypes
import Random

-- third party
import Data.List
import Data.Function
import GHC.Float


data HittableList = HList {objects :: NonEmptyList HittableObj}


instance Hittable HittableList where
    {-# INLINE hit #-}
    hit !hobj g !ry !tmin !tmax !hrec =
        let hs = nl2List $ objects hobj
            hitobjs = hits g tmax hs hrec -- [(hrec, Bool)]
        in if null hitobjs
           then (hrec, False, g)
           else minimumBy (compare `on` hrecDist) hitobjs
        where hits _ _ [] _ = []
              hits g1 mx (t:ts) hr =
                let (nhrec, isHit, g2) = hit t g1 ry tmin mx hr
                    nhdist = hdist nhrec
                in if isHit
                   then (nhrec, isHit, g2) : hits g2 nhdist ts nhrec
                   else hits g2 mx ts hr
              hrecDist (a, _, _) = hdist a

    boundingBox !hobjs !time0 !time1 !ab =
        let tempBox = zeroAabb3
            hs = nl2List $! objects hobjs
            firstBox = True
        in bbox firstBox time0 time1 tempBox hs ab
        where bbox False t0 t1 tbox [] outBox = (outBox, True)
              bbox fbox t0 t1 tbox (htl:htls) outBox =
                let (sbox, hasBox) = boundingBox htl t0 t1 tbox
                    result 
                        | not hasBox = (tbox, False)
                        | fbox = bbox False t0 t1 sbox htls sbox
                        | otherwise = let obox = ssBox outBox sbox
                                      in bbox False t0 t1 sbox htls obox
                in result

    pdf_value !hobjs gen !orig !dir =
        let weight = 1.0 / (int2Double $! lengthNL (objects hobjs))
            fn acc hobj = let RandResult (sumval, g) = acc 
                              resPval = pdf_value hobj g orig dir
                              res2 = rfmap (+ sumval) resPval
                          in rfmap (* weight) res2
            objs = nl2List $! objects hobjs
        in foldl fn (RandResult (0.0, gen)) objs

    hrandom !hobjs gen !orig =
        let upper = lengthNL $! objects hobjs
            -- randomInt produces random values in a closed range
            resIndex = randomInt gen (0, (upper - 1))
            RandResult (obj, g2) = rfmap (getNL (objects hobjs)) resIndex
        in hrandom obj g2 orig
