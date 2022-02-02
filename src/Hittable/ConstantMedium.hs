{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- constant medium module
module Hittable.ConstantMedium where

-- math
import Math3D.Vector
import Math3D.CommonOps
import Math3D.Ray

-- hittable
import Hittable.Hittable
import Hittable.HitRecord
import Hittable.Aabb

-- material
import Material.Material

-- texture
import Texture.Texture
import Texture.TextureObj

-- utility
import Utility.Utils

-- other stuff
import Prelude hiding(subtract)
import Random

data ConstantMedium where
    ConsMedium :: (Eq a, Show a, Hittable a) => a -> Material -> Double -> ConstantMedium

instance Eq ConstantMedium where
    a == b = 
        case a of
            (ConsMedium a1 _ d1) ->
                case b of
                    (ConsMedium a1 _ d2) -> a1 == a1 && (d1 == d2)

instance Show ConstantMedium where
    show a =
        case a of
            (ConsMedium boundary _ d) ->
                let msg1 = "<ConstantDensityVolume: boundary: " ++ show boundary
                    msg2 = " inverse negative density: " ++ show d ++ ">"
                in msg1 ++ msg2

--
mkConstantMedium :: (Eq a, Show a, Hittable a, Texture b) => a -> Double -> b -> ConstantMedium
mkConstantMedium !boundary !density !tex =
    ConsMedium boundary (IsotMat $ IsotTexture (TextureCons tex)) ((-1.0) / density)

instance Hittable ConstantMedium where
    {-# INLINE hit #-}
    boundingBox !a !tmn !tmx !ab =
        case a of
            (ConsMedium boundary _ _) -> boundingBox boundary tmn tmx ab

    -- hit function
    hit !a g !r !tmin !tmax !hrec =
        case a of
            (ConsMedium boundary mat invNegDensity) ->
                let (rec1, rec2) = (emptyRec, emptyRec) 
                    (nrec1, isFirstHit, g1) = hit boundary g r (-infty) infty rec1
                in if not isFirstHit
                   then (hrec, False, g1)
                   else let (nrec2, isSecHit, g2) = hit boundary g1 r ((hdist nrec1) + 0.0001) infty rec2
                        in if not isSecHit
                           then (hrec, False, g2) -- or (nrec1, False) ??
                           else let t_mn = if (hdist nrec1) < tmin
                                           then tmin
                                           else hdist nrec1
                                    t_min = if t_mn < 0
                                            then 0.0
                                            else t_mn
                                    t_max = if (hdist nrec2) > tmax
                                            then tmax
                                            else hdist nrec2
                                    rlength = magnitude $ direction r
                                    segmentLength = (t_max - t_min) * rlength
                                    RandResult (rval, g3) = randval g2
                                    hitDist = invNegDensity * (log rval)
                                in if hitDist > segmentLength
                                   then (hrec, False, g3)
                                   else let RandResult (hnorm, g4) = randomUnitVector g3
                                        in (HRec {
                                       hdist = t_min + hitDist / rlength,
                                       point = at r t_min,
                                       pnormal = hnorm,
                                       matPtr = mat,
                                       isFront = True, -- arbitrary
                                       hUV_u = hUV_u nrec1,
                                       hUV_v = hUV_v nrec1
                                       } ,
                                         True, g4)
    
    pdf_value _ g _ _ = RandResult (0.0, g)
    hrandom _ g _ = randomVec (0.0, 1.0) g
