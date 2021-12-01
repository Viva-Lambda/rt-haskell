{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-- rotatable
module Hittable.Rotatable where

import Hittable.Hittable
import Hittable.Aabb
import Hittable.HitRecord

-- math3d
import Math3D.Matrix
import Math3D.Vector
import Math3D.CommonOps
import Math3D.Ray

-- utility
import Utility.Utils

--
import Prelude hiding(subtract)
import Data.Foldable
import Data.List
import GHC.Float


data RotationAxis = RX
                  | RY
                  | RZ
                  deriving(Eq)

instance Show RotationAxis where
    show r = case r of
                RX -> "X"
                RY -> "Y"
                RZ -> "Z"

toMatrix :: RotationAxis -> Double -> Matrix
toMatrix r theta = case r of
                RX -> let matv1 = VList [1.0, 0.0, 0.0]
                          matv2 = VList [0.0, cos theta, -(sin theta)]
                          matv3 = VList [0.0, sin theta, cos theta]
                      in matFromVector [matv1, matv2, matv3]
                RY -> let matv1 = VList [cos theta, 0.0, sin theta]
                          matv2 = VList [0.0, 1.0, 0.0]
                          matv3 = VList [-(sin theta), 0.0, cos theta]
                      in matFromVector [matv1, matv2, matv3]
                RZ -> let matv1 = VList [cos theta, -(sin theta), 0.0]
                          matv2 = VList [sin theta, cos theta, 0.0]
                          matv3 = VList [0.0, 0.0, 1.0]
                      in matFromVector [matv1, matv2, matv3]

rotateByMatrix :: Vector -> Matrix -> Vector
rotateByMatrix (VList ps) rotmat =
    let pmat = MList {mdata = ps, mstride = 1}
        -- obtain rotated point vector
    in VList $ mdata (matmul rotmat pmat)


data Rotatable where
    Rotate :: (Show a, Hittable a, Eq a) => a -> Double -> RotationAxis -> Bool -> Aabb -> Rotatable

instance Show Rotatable where 
    show (Rotate a angle axis _ _) =
        let msg1 = "<Rotatable: " ++ show a ++ " angle: " ++ show angle
            msg2 = msg1 ++ " axis " ++ show axis ++ ">"
        in msg1 ++ msg2

instance Eq Rotatable where
    a == b =
        case a of
            (Rotate an angle axis _ _) -> 
                case b of
                    (Rotate an bngle bxis _ _) ->
                      (an == an) && (angle == bngle) && (axis == bxis)
                    _ -> False
            _ -> False

innerRotatable :: Int -> Int -> Int -> Aabb -> Matrix -> Vector -> Vector -> (Vector, Vector)
innerRotatable i j k bbox rotmat minv maxv =
    let mnmxMult key index = let bmax = (vget (aabbMax bbox) index) * key
                                 bmin = (vget (aabbMin bbox) index) * (1 - key)
                             in bmax + bmin
        xval = mnmxMult (int2Double i) 0
        yval = mnmxMult (int2Double j) 1
        zval = mnmxMult (int2Double k) 2
        -- make matrix for rotation
        pmat = MList {mdata = [xval, yval, zval], mstride = 1}
        -- obtain rotated point vector
        rotated = mdata (matmul rotmat pmat)
        VList mnvs = minv
        VList mxvs = maxv
        minvals = [minimum [mv, rv] | (mv, rv) <- zip rotated mnvs]
        maxvals = [maximum [mv, rv] | (mv, rv) <- zip rotated mxvs]
    in (VList minvals, VList maxvals)

mkRotatable :: (Show a, Hittable a, Eq a) => a -> Double -> RotationAxis -> Rotatable
mkRotatable ptr angle axis =
    let (ab, hasAb) = boundingBox ptr 0 1 zeroAabb3
        rotmat = toMatrix axis angle
        -- (a -> b -> a) -> [b] -> a
        kfoldfn i j = [(i,j,k) | k <- [0,1,2]]
        jfoldfn i = concat [kfoldfn i j | j <- [0,1,2]]
        inds = concat [jfoldfn i | i <- [0,1,2]]
        rotFold acc indices = let (i, j, k) = indices
                                  (minv, maxv) = acc
                              in innerRotatable i j k ab rotmat minv maxv
        minStart = inftyV3
        maxStart = negInftyV3
        (minv, maxv) = foldl' rotFold (minStart, maxStart) inds
        box = AaBbox {aabbMin = minv, aabbMax = maxv}
    in Rotate ptr angle axis hasAb box


instance Hittable Rotatable where
    hit (Rotate a angle axis _ _) g ry tmin tmax hrec =
        --
        let theta = degrees_to_radians angle
            rotmat = toMatrix axis theta
            invrot = toMatrix axis (-theta)
            ro = origin ry
            rd = direction ry
            rro = rotateByMatrix ro rotmat -- rotated origin
            rrd = rotateByMatrix rd rotmat -- rotated direction
            nry = Rd {origin = rro, direction = rrd, rtime = rtime ry}
            (srec, isHit, g1) = hit a g nry tmin tmax hrec
        in if not isHit
           then (srec, isHit, g1)
           else let invp = rotateByMatrix (point srec) invrot
                    invn = rotateByMatrix (pnormal srec) invrot
                    HRec {
                        hdist = h1, point = h2, pnormal = h3, matPtr = h4,
                        hUV_u = h5, hUV_v = h6, isFront = h7
                        } = srec
                    nsrec = HRec {hdist = h1, point = invp, pnormal = invn, 
                                  matPtr = h4, hUV_u = h5, hUV_v = h6,
                                  isFront = h7}
                in (setFaceNormal nsrec nry invn, True, g1)

    -- there is a problem here the bounding box does not care about the time
    -- TODO
    boundingBox (Rotate a angle axis hasBox bbox) tmn tmx ab = (bbox, hasBox)
