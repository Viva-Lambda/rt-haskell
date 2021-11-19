{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- bounding volume hierarchy
module Hittable.Bvh where

import Hittable.Hittable
import Hittable.HittableList
import Hittable.HittableObj
import Hittable.HitRecord
import Hittable.Aabb
import Data.List
import Math3D.Vector
import System.Random
import Random
import GHC.Float
import Data.Ord

data Bvh a where
    BNode :: Hittable a => Bvh a -> Bvh a -> Aabb -> Bvh a
    BLeaf :: Hittable a => a -> Aabb -> Bvh a
             

instance (Hittable a) => Hittable (Bvh a) where
    -- hit :: bvh -> Ray -> Double -> Double -> HitRecord -> (HitRecord, Bool)
    hit (BNode a b box) ray tmin tmax hrec =
        let boxHit = aabbHit box ray tmin tmax
        in if not boxHit
           then (hrec, False)
           else let (leftHrec, isLeftHit) = hit a ray tmin tmax hrec
                    t_max = if isLeftHit
                            then hdist leftHrec
                            else tmax
                    (rightHrec, isRightHit) = hit b ray tmin t_max hrec
                in if isRightHit
                   then (rightHrec, True)
                   else if isLeftHit
                        then (leftHrec, True)
                        else (hrec, False)
    hit (BLeaf a box) ray tmin tmax hrec = 
        if not $! aabbHit box ray tmin tmax
        then (hrec, False)
        else hit a ray tmin tmax hrec
                                            

    -- boundingBox :: bvh -> Double -> Double -> Aabb -> (Aabb, Bool)
    boundingBox (BNode _ _ a) t0 t1 obox = (a, True)
    boundingBox (BLeaf _ a) t0 t1 obox = (a, True)


mkBvh :: (Hittable a, RandomGen g) => [a] -> g -> Int -> Int -> Double -> Double -> Bvh a

mkBvh objects gen start end time0 time1 =
    let (axisd, g1) = randomDouble gen 0.0 2.0
        axis = double2Int axisd
        compareFn = if axis == 0
                    then box_x_compare
                    else if axis == 1
                         then box_y_compare
                         else box_z_compare
        object_span = end - start
        (leftObj, rightObj) = if object_span == 1 -- there is a single object
                              then let fobj = objects !! start
                                       (fBox, _) = boundingBox fobj 0.0 1.0 zeroAabb3
                                   in (BLeaf fobj fBox, BLeaf fobj fBox)
                              -- there are two objects
                              else if object_span == 2 
                                   then let fobj = objects !! start
                                            sobj = objects !! (start + 1)
                                            (fBox, _) = boundingBox fobj 0.0 1.0 zeroAabb3
                                            (sBox, _) = boundingBox sobj 0.0 1.0 zeroAabb3
                                        in case compareFn fobj sobj of
                                                LT -> (BLeaf fobj fBox, 
                                                       BLeaf sobj sBox)
                                                GT -> (BLeaf sobj sBox, 
                                                       BLeaf fobj fBox)
                                                EQ -> (BLeaf sobj sBox,
                                                       BLeaf fobj fBox)
                                   -- there are multiple objects
                                   else let {
        -- let's sort the given range
        indicesList = [0..(length objects)];
        objEnum = zip indicesList objects;
        lstComp = \(fInd, fObj) (sInd, sObj) -> 
                    if fInd < start
                    then EQ
                    else if sInd > end
                         then EQ
                         else compareFn fObj sObj;

        -- sortedEnumObjs :: [((Int, a), (Int, a))]
        sortedEnumObjs = sortBy lstComp objEnum;
        (indices, sortedObjs) = unzip sortedEnumObjs;

        spdouble = int2Double object_span;
        stdouble = int2Double start;
        middle = stdouble + (spdouble / 2);
        mid = double2Int middle;
        left = mkBvh sortedObjs g1 start mid time0 time1;
        right = mkBvh sortedObjs g1 mid end time0 time1;
        (lbox, _) = boundingBox left time0 time1 zeroAabb3;
        (rbox, _) = boundingBox right time0 time1 zeroAabb3;
                                            }
                                        in (left, right)
        -- we have the left and right branch, now aabb
        (leftBox, isLBox) = boundingBox leftObj time0 time1 zeroAabb3
        (rightBox, isRBox) = boundingBox rightObj time0 time1 zeroAabb3
    in if (not isLBox) || (not isRBox)
       then error "bvh node does not have a bounding box"
       else let bvhBox = ssBox leftBox rightBox
            in BNode leftObj rightObj bvhBox
    where boxCompare firstObject secondObject compareAxis =
            let (fBox, isfBox) = boundingBox firstObject 0.0 1.0 zeroAabb3
                (sBox, issBox) = boundingBox secondObject 0.0 1.0 zeroAabb3
            in if (not isfBox) || (not issBox)
               then error "bvh node does not have a bounding box in compare"
               else let fVal = vget (aabbMin fBox) compareAxis
                        sVal = vget (aabbMin sBox) compareAxis
                    in if fVal < sVal
                       then LT
                       else if sVal < fVal
                            then GT
                            else EQ

          box_x_compare f s = boxCompare f s 0
          box_y_compare f s = boxCompare f s 1
          box_z_compare f s = boxCompare f s 2

mkBvhFromHittableList :: RandomGen g => HittableList -> g -> Double -> Double -> Bvh HittableObj

mkBvhFromHittableList
    (HList hs) g time0 time1 = mkBvh hs g 0 (length hs) time0 time1
