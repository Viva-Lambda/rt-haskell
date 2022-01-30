{-# LANGUAGE BangPatterns #-}
-- module for sphere objects
module Hittable.Sphere where

import Math3D.Vector
import Math3D.CommonOps
import Math3D.Ray
import Math3D.Onb

import Utility.Utils
import Hittable.Hittable
import Hittable.HitRecord
import Hittable.Aabb
import Prelude hiding(subtract)
import Material.Material

-- random
import Random


data Sphere = SphereObj {sphereCenter :: Vector,
                         sphereRadius :: Double, 
                         sphereMat :: Material}

getSphereUV :: Vector -> (Double, Double)
getSphereUV v =
    let (x:y:z:_) = vec2List v
        theta = acos (-y)
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
                     sphereMat = sm}) g !(Rd {origin = ro,
                                             direction = rd,
                                             rtime = rt,
                                             wavelength = rwave }) !tmin !tmax !hrec =
        let oc = subtract ro sc
            a = lengthSquared rd
            hb = dot oc rd
            c = (lengthSquared oc) - (sr * sr)
            discriminant = hb * hb - a * c
            ry = Rd {origin = ro, direction = rd,
                     rtime = rt, wavelength = rwave}
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

    boundingBox !(SphereObj {sphereCenter = sc, sphereRadius = sr,
                            sphereMat = _}) !tmn !tmx !ab =
        let cv1 = subtract sc (fromList2Vec sr [sr, sr])
            cv2 = add sc (fromList2Vec sr [sr, sr])
            aBound = AaBbox { aabbMin = cv1, aabbMax = cv2 }
        in (aBound, True)

    pdf_value a g orig v =
        let hr = emptyRec
            ry = Rd {origin = orig, direction = v, 
                     rtime = 0.0, wavelength = 0}
            (ahit, isHit, g1) = hit a g ry 0.001 (infty) hr
        in if not isHit
           then RandResult (0.0, g1)
           else let cent = sphereCenter a
                    corg = lengthSquared $! subtract cent orig
                    radius = sphereRadius a
                    radorg = (radius * radius) / corg
                    costheta = sqrt $! 1.0 - radorg
                    solidAngle = 2.0 * m_pi * (1.0 - costheta)
                in RandResult (1.0 / solidAngle, g1)

    hrandom a g orig =
        let cent = sphereCenter a
            dir = subtract cent orig
            distSqr = lengthSquared dir
            onb = fromW2Onb dir
            radius = sphereRadius a
            res = random2Sphere g (radius, distSqr)
        in rfmap (localVec onb) res

