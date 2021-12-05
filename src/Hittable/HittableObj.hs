{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- hittable object module
module Hittable.HittableObj where

import Hittable.Hittable
import Hittable.Sphere
import Hittable.MovingSphere
import Hittable.AaRect
import Hittable.Rotatable
import Hittable.Translatable

-- 
import Math3D.Vector

--
import Instance.Box

data HittableObj where
    HittableCons :: (Eq a, Show a, Hittable a) => a -> HittableObj

instance Eq HittableObj where
    a == b = 
        case a of
            HittableCons a1 ->
                case b of
                    HittableCons a1 -> a1 == a1
                    _ -> False
            _ -> False

instance Show HittableObj where
    show a =
        case a of
            HittableCons a1 -> show a1



instance Hittable HittableObj where
    {-# INLINE hit #-}
    hit hobj g !ry !tmin !tmax !hrec =
        case hobj of
            (HittableCons a) -> hit a g ry tmin tmax hrec

    boundingBox hobj time0 time1 ab =
        case hobj of
            (HittableCons s) -> boundingBox s time0 time1 ab

    pdf_value hobj g o v =
        case hobj of
            (HittableCons s) -> pdf_value s g o v

    hrandom hobj g v =
        case hobj of
            (HittableCons s) -> hrandom s g v
