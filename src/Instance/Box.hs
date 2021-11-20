{-# LANGUAGE BangPatterns #-}
-- box instance for holding quads
module Instance.Box where

-- math3d
import Math3D.Vector
import Math3D.Ray
import Math3D.Transform

import Hittable.AaRect
import Hittable.Hittable
import Hittable.HitRecord
import Hittable.Aabb

import Data.List
import Data.Function

import Material.Material
import Utility.HelperTypes

data Box = HBox { minBox :: Vector, maxBox :: Vector,
                  boxMat :: Material, boxSides :: [AaRect] }

mkBox :: Vector -> Vector -> Material -> Box
mkBox mn mx mat =
    let mnx = vget mn 0
        mny = vget mn 1
        mnz = vget mn 2
        mxx = vget mx 0
        mxy = vget mx 1
        mxz = vget mx 2
        --
        s1 = mkXyRect mnx mxx mny mxy mxz mat
        s2 = mkXyRect mnx mxx mny mxy mnz mat
        --
        s3 = mkXzRect mnx mxx mnz mxz mny mat
        s4 = mkXzRect mnx mxx mnz mxz mxy mat
        --
        s5 = mkYzRect mny mxy mnz mxz mnx mat
        s6 = mkYzRect mny mxy mnz mxz mxx mat
    in HBox {minBox = mn, maxBox = mx, boxMat = mat,
             boxSides = [s1,s2,s3,s4,s5,s6]}

instance Eq Box where
    a == b = 
        let HBox {minBox = amin, maxBox = amax, boxSides = as} = a
            HBox {minBox = bmin, maxBox = bmax, boxSides = bs} = b
        in amin == bmin && amax == bmax && as == bs

instance Show Box where
    show (HBox {minBox = amin, maxBox = amax, boxSides = as}) =
        let msg = "<Box: min: " ++ show amin ++ " "
            msg2 = "max: " ++ show amax ++ " >"
        in msg ++ msg2

instance Hittable Box where
    {-# INLINE hit #-}
    hit a ry tmin tmax hrec =
        let (e:es) = boxSides a
            hitobjs = hits tmax (e:es) hrec -- [(hrec, Bool)]
        in if null hitobjs
           then (hrec, False)
           else minimumBy (compare `on` hrecDist) hitobjs
        where hits _ [] _ = []
              hits mx (t:ts) hr =
                let (nhrec, isHit) = hit t ry tmin mx hr
                    nhdist = hdist nhrec
                in if isHit
                   then (nhrec, isHit) : hits nhdist ts nhrec
                   else hits mx ts hr
              hrecDist (a, _) = hdist a

    boundingBox !a !time0 !time1 !ab =
        let hs = boxSides a
        in if null hs
           then (ab, False)
           else let tempBox = zeroAabb3
                    firstBox = True
                in bbox firstBox time0 time1 tempBox hs ab
                where bbox False t0 t1 tbox [] outBox = (outBox, True)
                      bbox fbox t0 t1 tbox (htl:htls) outBox =
                        let (sbox, hasBox) = boundingBox htl t0 t1 tbox
                        in if hasBox == False
                           then (tbox, False)
                           else if fbox
                                then bbox False t0 t1 sbox htls sbox
                                else let obox = ssBox outBox sbox
                                     in bbox False t0 t1 sbox htls obox


instance Transformable Box where
    transform b nlocs =
        let bmat = boxMat b
        in case nlocs of
                (NList a [c]) -> mkBox a c bmat
                _ -> error $ "transform is not possible " ++ show (lengthNL nlocs)

instance Locatable Box where
    --
    localCoords a _ = NList (minBox a) [maxBox a]

instance Translatable Box where

instance Rotatable Box where
