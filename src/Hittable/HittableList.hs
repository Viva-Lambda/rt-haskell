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
        let hs = toList $ objects hobj
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

    pdf_value hobjs gen orig dir =
        let weight = 1.0 / (int2Double $ lengthNL (objects hobjs))
            fn acc hobj = let (sumval, g) = acc 
                              (pdfval, g2) = (pdf_value hobj g orig dir) 
                              pval = pdfval * weight
                          in (sumval + pval, g2)
            objs = toList $! objects hobjs
        in foldl fn (0.0, gen) objs

    hrandom hobjs gen orig =
        let upper = lengthNL $! objects hobjs
            (index, g2) = randomInt gen 0 upper
        in hrandom (getNL (objects hobjs) index) g2 orig
