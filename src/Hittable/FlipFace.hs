{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-- rotatable
module Hittable.FlipFace where

-- hittable
import Hittable.Hittable
import Hittable.Aabb
import Hittable.HitRecord

-- math
import Math3D.Vector
import Math3D.CommonOps

-- thirdparty
import Prelude hiding(subtract)

data FlipFace where
    FlipHittable :: (Show a, Hittable a, Eq a) => a -> String -> FlipFace

instance Show FlipFace where
    show (FlipHittable a _) = let msg1 = "<FlipFace: " ++ show a in msg1 ++ " >"

instance Eq FlipFace where
    (FlipHittable _ a) == (FlipHittable _ b) = a == b

instance Hittable FlipFace where
    hit (FlipHittable a _) g !ry !tmin !tmax !hrec =
        let (srec, isHit, g1) = hit a g ry tmin tmax hrec
        in if not isHit
           then (srec, isHit, g1)
           else (HRec {
               point = point srec,
               pnormal = pnormal srec,
               hdist = hdist srec,
               hUVu = hUVu srec,
               hUVv = hUVv srec,
               matPtr = matPtr srec,
               isFront = not $! isFront srec
               }, True, g1)

    boundingBox (FlipHittable a _) = boundingBox a
    pdf_value a g orig v = 
        case a of 
            FlipHittable b _ -> pdf_value b g orig v
    hrandom a g orig =
        case a of
            FlipHittable b _ -> hrandom b g orig
