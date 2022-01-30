{-# LANGUAGE BangPatterns #-}
-- moving sphere module
module Hittable.MovingSphere where

-- math
import Math3D.Ray
import Math3D.Vector
import Math3D.CommonOps
import Math3D.Onb

-- hittable
import Hittable.Hittable
import Hittable.HitRecord
import Hittable.Aabb

-- random
import Random

-- material
import Material.Material

import Utility.Utils
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


getSphereUV :: Vector -> (Double, Double)
getSphereUV v = 
    let (x: y: z:_) = vec2List v
        theta = acos (-y)
        phi = (atan2 (-z) x) + m_pi
    in (phi / (2*m_pi), theta / m_pi)


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
    hit !s g !(Rd {origin = ro, rtime = t0,
                   direction = rd, wavelength = rwave}) !tmin !tmax !hrec =
        let sr = msphereRadius s
            sm = msphereMat s
            sc = (getMSphereCenter s t0)
            oc = subtract ro sc
            a = lengthSquared rd
            hb = dot oc rd
            c = (lengthSquared oc) - (sr * sr)
            discriminant = hb * hb - a * c
            ry = Rd {origin = ro, direction = rd, rtime = t0,
                     wavelength = rwave}
        in if discriminant < 0
           then (hrec, False, g)
           else let sqd = sqrt discriminant
                    root = (-hb - sqd) / a
                    nroot = (-hb + sqd) / a
                    cond1 = root < tmin || tmax < root
                    cond2 = nroot < tmin || tmax < nroot
                in if cond1
                   then if cond2
                        then (hrec, False, g)
                        else let hpoint = at ry nroot
                                 hnorm = divideS (subtract hpoint sc) sr
                                 (hu, hv) = getSphereUV hnorm
                                 hr = HRec {hdist = nroot, point = hpoint,
                                            pnormal = hnorm,
                                            matPtr = sm,
                                            hUV_u = hu,
                                            hUV_v = hv,
                                            isFront = False}
                             in (setFaceNormal hr ry hnorm, True, g)
                   else let hpoint = at ry root
                            hnorm = divideS (subtract hpoint sc) sr
                            (hu, hv) = getSphereUV hnorm
                            hr = HRec {hdist = root, point = hpoint,
                                       pnormal = hnorm, matPtr = sm,
                                       hUV_u = hu, hUV_v = hv,
                                       isFront = False}
                        in (setFaceNormal hr ry hnorm, True, g)

    boundingBox !s !time0 !time1 !ab =
        let ct0 = getMSphereCenter s time0
            ct1 = getMSphereCenter s time1
            srad = msphereRadius s
            vrad = fromList2Vec srad [srad, srad]
            ab1 = AaBbox {aabbMin = subtract ct0 vrad, aabbMax = add ct0 vrad}
            ab2 = AaBbox {aabbMin = subtract ct1 vrad, aabbMax = add ct1 vrad}
        in (ssBox ab1 ab2, True)

    pdf_value a g orig v = 
        let hr = emptyRec
            ry = Rd {origin = orig, direction = v,
                     rtime = 0.0, wavelength = 0}
            (ahit, isHit, g1) = hit a g ry 0.001 (infty) hr
        in if not isHit
           then RandResult (0.0, g1)
           else let cent = getMSphereCenter a 0.0
                    corg = lengthSquared $! subtract cent orig
                    radius = msphereRadius a
                    radorg = (radius * radius) / corg
                    costheta = sqrt $! 1.0 - radorg
                    solidAngle = 2.0 * m_pi * (1.0 - costheta)
                in RandResult (1.0 / solidAngle, g1)

    hrandom a g orig =
        let cent = getMSphereCenter a 0.0
            dir = subtract cent orig
            distSqr = lengthSquared dir
            onb = fromW2Onb dir
            radius = msphereRadius a
            res = random2Sphere g (radius, distSqr)
        in rfmap (localVec onb) res

