{-# LANGUAGE BangPatterns #-}
-- hittable object module
module Hittable.HittableObj where

import Hittable.Hittable
import Hittable.Sphere
import Hittable.MovingSphere
import Hittable.AaRect
import Hittable.Rotatable
import Hittable.Translatable

--
import Instance.Box

data HittableObj = HitSphere !Sphere 
                 | MvHitSphere !MovingSphere
                 | AaQuad AaRect
                 | HitBox Box
                 | HRotate Rotatable
                 | HTranslate Translatable
                 deriving (Show, Eq)

instance Hittable HittableObj where
    hit hobj !ry !tmin !tmax !hrec =
        case hobj of
            HitSphere s -> hit s ry tmin tmax hrec
            MvHitSphere s -> hit s ry tmin tmax hrec
            AaQuad s -> hit s ry tmin tmax hrec
            HitBox s -> hit s ry tmin tmax hrec
            HRotate s -> hit s ry tmin tmax hrec
            HTranslate s -> hit s ry tmin tmax hrec

    boundingBox hobj time0 time1 ab =
        case hobj of
            HitSphere s -> boundingBox s time0 time1 ab
            MvHitSphere s -> boundingBox s time0 time1 ab
            AaQuad s -> boundingBox s time0 time1 ab
            HitBox s -> boundingBox s time0 time1 ab
            HRotate s -> boundingBox s time0 time1 ab
            HTranslate s -> boundingBox s time0 time1 ab
