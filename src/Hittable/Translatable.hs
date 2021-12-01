{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-- rotatable
module Hittable.Translatable where

import Hittable.Hittable
import Hittable.Aabb
import Hittable.HitRecord

-- math3d
import Math3D.Matrix
import Math3D.Vector
import Math3D.CommonOps
import Math3D.Ray
import Prelude hiding(subtract)


data Translatable where
    Translate :: (Show a, Hittable a, Eq a) => a -> Vector -> Translatable

instance Show Translatable where
    show (Translate a offset) = 
        let msg1 = "<Translatable: " ++ show a ++ " offset " ++ show offset
        in msg1 ++ " >"

instance Eq Translatable where
    a == b = case a of
                Translate an aoff ->
                    case b of
                        Translate an boff ->
                            (an == an) && (aoff == boff)
                        _ -> False
                _ -> False

instance Hittable Translatable where
    hit (Translate a offset) g !(Rd {origin = ro, 
                                   direction = rd,
                                   rtime = rt}) !tmin !tmax !hrec =
        let ry = Rd {origin = subtract ro offset, direction = rd, rtime = rt}
            (srec, isHit, g1) = hit a g ry tmin tmax hrec
        in if not isHit
           then (srec, isHit, g1)
           else let p = add (point srec) offset
                    HRec {
                        hdist = h1, point = h2, pnormal = h3, matPtr = h4,
                        hUV_u = h5, hUV_v = h6, isFront = h7
                        } = srec
                    nsrec = HRec {hdist = h1, point = p, pnormal = h3, 
                                  matPtr = h4, hUV_u = h5, hUV_v = h6,
                                  isFront = h7}
                in (setFaceNormal nsrec ry h3, True, g1)

    boundingBox (Translate a offset) tmn tmx ab =
        let (abound, isBox) = boundingBox a tmn tmx ab
        in if not isBox
           then (abound, isBox)
           else let amin = add (aabbMin abound) offset
                    amax = add (aabbMax abound) offset
                in (AaBbox {aabbMin = amin, aabbMax = amax}, True)
