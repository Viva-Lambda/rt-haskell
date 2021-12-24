{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- bounding volume hierarchy
module Hittable.Bvh where

import Hittable.Hittable
import Hittable.HittableList
import Hittable.HittableObj
import Hittable.HitRecord
import Hittable.Aabb

import Math3D.Vector

import Utility.HelperTypes

import Random
import GHC.Float
import System.Random
import Data.Ord
import Data.List

data Bvh where
    BNode :: Bvh -> Bvh -> Aabb -> Bvh
    BLeaf :: (Show a, Eq a, Hittable a) => a -> Aabb -> Bvh

instance Eq Bvh where
    a == b = 
        case a of
            (BNode _ _ ab) -> 
                case b of
                    (BNode _ _ ab2) -> ab == ab2
                    _ -> False
            (BLeaf _ ab) ->
                case b of
                    (BLeaf _ ab2) -> ab == ab2
                    _ -> False

instance Show Bvh where
    show a = case a of
                (BNode a1 a2 _) ->
                    let msg = "<BVH Node: Left: " ++ show a1
                        msg2 = " Right: " ++ show a2 ++ ">"
                    in msg ++ msg2
                (BLeaf a1 _) -> "<BVH Leaf: " ++ show a1 ++ ">"
             

instance Hittable Bvh where
    {-# INLINE hit #-}
    -- hit :: bvh -> randgen -> Ray -> Double -> Double -> HitRecord -> (HitRecord, Bool)
    hit !bvh !g !ray !tmin !tmax !hrec =
        case bvh of
            (BNode a b box) ->
                let boxHit = aabbHit box ray tmin tmax
                in if not boxHit
                   then (hrec, False, g)
                   else let (leftHrec, isLeftHit, g1) = hit a g ray tmin tmax hrec
                            t_max = if isLeftHit
                                    then hdist leftHrec
                                    else tmax
                            (rightHrec, isRightHit, g2) = hit b g1 ray tmin t_max hrec
                        in if isRightHit
                           then (rightHrec, True, g2)
                           else if isLeftHit
                                then (leftHrec, True, g1)
                                else (hrec, False, g)
            (BLeaf a box) ->
                if not $! aabbHit box ray tmin tmax
                then (hrec, False, g)
                else hit a g ray tmin tmax hrec
                                            

    -- boundingBox :: bvh -> Double -> Double -> Aabb -> (Aabb, Bool)
    boundingBox mbvh  _ _ _ = 
        case mbvh of 
            (BNode _ _ a) -> (a, True)
            (BLeaf _ a) -> (a, True)

    -- pdf value
    pdf_value _ g _ _ = RandResult (0.0, g)
    hrandom _ g _ = randomVec (0.0, 1.0) g


mkBvh :: (Show a, Eq a, Hittable a, RandomGen g) => [a] -> g -> Int -> Int -> Double -> Double -> Bvh

mkBvh !objects !gen !start !end !time0 !time1 =
    let resAxis = randomDouble gen (0.0, 2.0)
        RandResult (axis, g1) = rfmap double2Int resAxis
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

mkBvhFromHittableList :: RandomGen g => HittableList -> g -> Double -> Double -> Bvh

mkBvhFromHittableList hobjs g time0 time1 =
    let hs = nl2List $ objects hobjs
    in mkBvh hs g 0 (length hs) time0 time1
