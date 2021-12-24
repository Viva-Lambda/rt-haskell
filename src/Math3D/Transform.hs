-- 3d transformations module
module Math3D.Transform where

import Math3D.Vector
import Math3D.CommonOps
import Math3D.Onb
import Math3D.Quaternion
import Math3D.Matrix
import Utility.HelperTypes
import Utility.Utils

import Prelude hiding(add)


type Origin = Vector
type Point = Vector
type Offset = Vector
type LocatingParams = (Origin, OrthoNormalBase, Double)
type AxisAngle = (Vector, Double)


class Transformable a where
    transform :: a -> NonEmptyList Vector -> a

locate :: Vector -> Origin -> OrthoNormalBase -> Vector
locate avec origin onb = 
    let lvec = localVec onb avec
        lorg = add origin lvec
    in lorg

class Transformable a => Locatable a where
    localCoords :: a -> Double -> NonEmptyList Vector
    
    located :: a -> LocatingParams -> a
    located a (origin, onb, time) =
        let func v = locate v origin onb
            locs = mapNL func (localCoords a time)
        in transform a locs


class (Show a, Locatable a, Transformable a) => Translatable a where
    translate :: a -> Offset -> Double -> a
    translate a offset time =
        let fnc vec = add vec offset
            nlocs = mapNL fnc (localCoords a time)
        in transform a nlocs


class (Locatable a, Transformable a, Show a) => Rotatable a where
    rotate :: a -> LocatingParams -> Quaternion -> a
    {-
    Rotate a point with a quaternion, from:
    Vince, J. (2011) Quaternions for Computer Graphics. London: Springer London.
    q p q^{ -1 }
    -}
    rotate a (origin, onb, time) quat =
        let -- locs = [ locate v origin onb | v <- (toList $ localCoords a time) ]
            locs = localCoords a time
            rotatePoint point =
                let pquat = fromSVec2Quaternion 0.0 point
                    qinv = qInverse quat
                in hamiltonProduct qinv (hamiltonProduct pquat quat) 
            nlocs = mapNL qVector $! mapNL rotatePoint locs
        in transform a nlocs

    rotateMatAngle :: a -> Double -> Double -> Matrix -> a
    rotateMatAngle a angleDegree time rotmat =
        let 
            locs = localCoords a time
            -- 3x1 vector
            toMatrix v = let (v1, v2, v3) = ((vget v 0), (vget v 1), (vget v 2))
                         in MList {mdata = fromList2NL v1 [v2, v3], mstride = 1}
            matList = mapNL toMatrix locs
            mmul m = matmul rotmat m
            rotatedCoords = mapNL mmul matList
            -- mat to vector
            toVector m = VList $ mdata m
            nlocs = mapNL toVector rotatedCoords
            nobj = transform a nlocs
        in nobj

    rotateXByAngle :: a -> Double -> Double -> a
    rotateXByAngle a angleDegree time =
        let theta = degrees_to_radians angleDegree
            matv1 = fromList2Vec 1.0 [0.0, 0.0]
            matv2 = fromList2Vec 0.0 [cos theta, -(sin theta)]
            matv3 = fromList2Vec 0.0 [sin theta, cos theta]
            rotmat = matFromVector (fromList2NL matv1 [matv2, matv3]) -- 3x3 matrix
        in rotateMatAngle a angleDegree time rotmat

    rotateYByAngle :: a -> Double -> Double -> a
    rotateYByAngle a angleDegree time =
        let theta = degrees_to_radians angleDegree
            matv1 = fromList2Vec (cos theta) [0.0, sin theta]
            matv2 = fromList2Vec 0.0 [1.0, 0.0]
            matv3 = fromList2Vec (-(sin theta)) [0.0, cos theta]
            rotmat = matFromVector (fromList2NL matv1 [matv2, matv3]) -- 3x3 matrix
        in rotateMatAngle a angleDegree time rotmat

    rotateZByAngle :: a -> Double -> Double -> a
    rotateZByAngle a angleDegree time =
        let theta = degrees_to_radians angleDegree
            matv1 = fromList2Vec (cos theta) [-(sin theta), 0.0]
            matv2 = fromList2Vec (sin theta) [cos theta, 0.0]
            matv3 = fromList2Vec 0.0 [0.0, 1.0]
            rotmat = matFromVector (fromList2NL matv1 [matv2, matv3]) -- 3x3 matrix
        in rotateMatAngle a angleDegree time rotmat


class Scalable a where
    scale :: (Locatable a, Transformable a) => a -> Offset -> LocatingParams -> a
    scale a offset (origin, onb, time) =
        let locs = [locate v origin onb | v <- (nl2List $ localCoords a time)]
            (n:ns) = [multiply offset v | v <- locs]
        in transform a (fromList2NL n ns)
