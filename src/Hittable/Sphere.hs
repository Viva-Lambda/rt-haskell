{-# LANGUAGE BangPatterns #-}
-- module for sphere objects
module Hittable.Sphere where

import Vector
import Utility.Utils
import Hittable.Hittable
import Hittable.HitRecord
import Hittable.Aabb
import Ray
import Prelude hiding(subtract)
import Material.Material


data Sphere  = SphereObj {sphereCenter :: Vector,
                          sphereRadius :: Double, 
                          sphereMat :: Material}

getSphereUV :: Vector -> (Double, Double)
getSphereUV (VList [x, y, z]) = 
    let theta = acos (-y)
        phi = (atan2 (-z) x) + m_pi
    in (phi / (2*m_pi), theta / m_pi)

instance Eq Sphere where
    (SphereObj {sphereCenter = a,
                sphereRadius = b}) == (SphereObj {sphereCenter = c,
                                                  sphereRadius = d}) =
        a == c && b == d

instance Show Sphere where
    show (SphereObj {sphereCenter = a, sphereRadius = b}) =
        "Shpere with center: " ++ show a ++ " radius: " ++ show b


instance Hittable Sphere where
    {-# INLINE hit #-}
    hit !(SphereObj {sphereCenter = sc,
                    sphereRadius = sr,
                    sphereMat = sm}) !(Rd {origin = ro, 
                                           direction = rd,
                                           rtime = rt}) !tmin !tmax !hrec =
        let oc = subtract ro sc
            a = lengthSquared rd
            hb = dot oc rd
            c = (lengthSquared oc) - (sr * sr)
            discriminant = hb * hb - a * c
            ry = Rd {origin = ro, direction = rd, rtime = rt}
        in if discriminant < 0
           then (hrec, False)
           else let sqd = sqrt discriminant
                    root = (-hb - sqd) / a
                    nroot = (-hb + sqd) / a
                    cond1 = root < tmin || tmax < root
                    cond2 = nroot < tmin || tmax < nroot
                in if cond1
                   then if cond2
                        then (hrec, False)
                        else let hpoint = at ry nroot
                                 hnorm = divideS (subtract hpoint sc) sr
                                 (hu, hv) = getSphereUV hnorm
                                 hr = HRec {hdist = nroot, point = hpoint,
                                            pnormal = hnorm,
                                            matPtr = sm,
                                            hUV_u = hu,
                                            hUV_v = hv,
                                            isFront = False}
                             in (setFaceNormal hr ry hnorm, True)
                   else let hpoint = at ry root
                            hnorm = divideS (subtract hpoint sc) sr
                            (hu, hv) = getSphereUV hnorm
                            hr = HRec {hdist = root, point = hpoint,
                                       pnormal = hnorm, matPtr = sm,
                                       hUV_u = hu, hUV_v = hv,
                                       isFront = False}
                        in (setFaceNormal hr ry hnorm, True)

    boundingBox !(SphereObj {sphereCenter = sc, sphereRadius = sr,
                            sphereMat = _}) !tmn !tmx !ab =
        let cv1 = subtract sc (VList [sr, sr, sr])
            cv2 = add sc (VList [sr, sr, sr])
            aBound = AaBbox { aabbMin = cv1, aabbMax = cv2 }
        in (aBound, True)
