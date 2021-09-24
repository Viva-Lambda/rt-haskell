-- module for sphere objects
module Hittable.Sphere where

import Vector
import Hittable.Hittable
import Ray
import Prelude hiding(subtract)


data Sphere  = SphereObj {sphereCenter :: Vector, sphereRadius :: Double}

instance Eq Sphere where
    (SphereObj {sphereCenter = a,
                sphereRadius = b}) == (SphereObj {sphereCenter = c,
                                                  sphereRadius = d}) =
        a == c && b == d

instance Show Sphere where
    show (SphereObj {sphereCenter = a, sphereRadius = b}) =
        "Shpere with center: " ++ show a ++ " radius: " ++ show b


instance Hittable Sphere where
    hit (SphereObj {sphereCenter = sc,
                    sphereRadius = sr}) (Rd {origin = ro, 
                                             direction = rd}) tmin tmax hrec =
        let oc = subtract ro sc
            a = lengthSquared rd
            hb = dot oc rd
            c = (lengthSquared oc) - (sr * sr)
            discriminant = hb * hb - a * c
            ry = Rd {origin = ro, direction = rd}
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
                                 hr = HRec {dist = nroot, point = hpoint,
                                            pnormal = hnorm}
                             in (setFaceNormal hr ry hnorm, True)
                   else let hpoint = at ry root
                            hnorm = divideS (subtract hpoint sc) sr
                            hr = HRec {dist = root, point = hpoint,
                                       pnormal = hnorm}
                        in (setFaceNormal hr ry hnorm, True)
