{-# LANGUAGE BangPatterns #-}
-- moving sphere module
module Hittable.MovingSphere where

import Ray
import Vector
import Hittable.Hittable
import Hittable.HitRecord
import Hittable.Aabb
import Material.Material
import Prelude hiding (subtract)

data MovingSphere = MovSphereObj {msphereCenter1 :: Vector,
                                  msphereCenter2 :: Vector,
                                  msphereRadius :: Double, 
                                  msphereMat :: Material,
                                  mTime0 :: Double,
                                  mTime1 :: Double}

getMSphereCenter :: MovingSphere -> Double -> Vector
getMSphereCenter !(MovSphereObj {msphereCenter1 = a,
                               msphereCenter2 = b,
                               msphereRadius = c,
                               msphereMat = d,
                               mTime0 = e,
                               mTime1 = f }) !time =
    let tratio = (time - e) / (f - e)
        centerDiff = subtract b a
        mc = multiplyS centerDiff tratio
    in add a mc


instance Eq MovingSphere where
    a == b =
        let MovSphereObj {msphereCenter1 = c, msphereCenter2 = d,
                          msphereRadius = e, msphereMat = _} = a
            MovSphereObj {msphereCenter1 = g, msphereCenter2 = h,
                          msphereRadius = i, msphereMat = _} = b
        in (c == g) && (d == h) && (e == i) -- && (f == j)

instance Show MovingSphere where
    show (MovSphereObj {msphereCenter1 = a,
                        msphereCenter2 = b,
                        msphereRadius = c,
                        msphereMat = _}) =
        let m1 = "Moving Shpere with "
            m2 = m1 ++ "center1 " ++ show a
            m3 = m2 ++ " center2 " ++ show b
            m4 = m3 ++ " radius " ++ show c
            -- m5 = m4 ++ " material " ++ show d
        in m4


instance Hittable MovingSphere where
    {-# INLINE hit #-}
    hit !s !(Rd {origin = ro, rtime = t0, direction = rd}) !tmin !tmax !hrec =
        let sr = msphereRadius s
            sm = msphereMat s
            sc = (getMSphereCenter s t0)
            oc = subtract ro sc
            a = lengthSquared rd
            hb = dot oc rd
            c = (lengthSquared oc) - (sr * sr)
            discriminant = hb * hb - a * c
            ry = Rd {origin = ro, direction = rd, rtime = t0}
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
                                 hr = HRec {hdist = nroot, point = hpoint,
                                            pnormal = hnorm,
                                            matPtr = sm,
                                            isFront = False}
                             in (setFaceNormal hr ry hnorm, True)
                   else let hpoint = at ry root
                            hnorm = divideS (subtract hpoint sc) sr
                            hr = HRec {hdist = root, point = hpoint,
                                       pnormal = hnorm, matPtr = sm, 
                                       isFront = False}
                        in (setFaceNormal hr ry hnorm, True)

    boundingBox !s !time0 !time1 !ab =
        let ct0 = getMSphereCenter s time0
            ct1 = getMSphereCenter s time1
            srad = msphereRadius s
            vrad = VList [srad, srad, srad]
            ab1 = AaBbox {aabbMin = subtract ct0 vrad, aabbMax = add ct0 vrad}
            ab2 = AaBbox {aabbMin = subtract ct1 vrad, aabbMax = add ct1 vrad}
        in (ssBox ab1 ab2, True)
