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
    Translate :: (Show a, Hittable a, Eq a) => a -> Vector -> String -> Translatable

instance Show Translatable where
    show (Translate a offset _) = 
        let msg1 = "<Translatable: " ++ show a ++ " offset " ++ show offset
        in msg1 ++ " >"

instance Eq Translatable where
    a == b = case a of
                Translate _ _ an ->
                    case b of
                        Translate _ _ bn -> (an == bn)

instance Hittable Translatable where
    hit (Translate a offset _) g !(Rd {origin = ro, 
                                     direction = rd,
                                     rtime = rt,
                                     wavelength = rwave}) !tmin !tmax !hrec =
        let ry = Rd { origin = subtract ro offset, direction = rd,
                      rtime = rt, wavelength = rwave }
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

    boundingBox (Translate a offset _) tmn tmx ab =
        let (abound, isBox) = boundingBox a tmn tmx ab
        in if not isBox
           then (abound, isBox)
           else let amin = add (aabbMin abound) offset
                    amax = add (aabbMax abound) offset
                in (AaBbox {aabbMin = amin, aabbMax = amax}, True)

    pdf_value a g orig v = 
        case a of 
            Translate b oset _ -> pdf_value b g (subtract orig oset) v
    hrandom a g orig = 
        case a of
            Translate b oset _ -> hrandom b g (subtract orig oset)
