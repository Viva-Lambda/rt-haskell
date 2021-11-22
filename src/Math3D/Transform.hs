-- 3d transformations module
module Math3D.Transform where

import Math3D.Vector
import Math3D.CommonOps
import Math3D.Onb
import Math3D.Quaternion
import Utility.HelperTypes

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
            locs = map func $ toList (localCoords a time)
        in transform a (NList (head locs) (tail locs))


class (Show a, Locatable a, Transformable a) => Translatable a where
    translate :: a -> Offset -> LocatingParams -> a
    translate a offset (origin, onb, time) =
        let fnc vec = add vec offset
            locs = toList (localCoords a time)
            nlocs = map fnc locs
        in transform a (NList (head nlocs) (tail nlocs))


class (Locatable a, Transformable a, Show a) => Rotatable a where
    rotate :: a -> LocatingParams -> Quaternion -> a
    {-
    Rotate a point with a quaternion, from:
    Vince, J. (2011) Quaternions for Computer Graphics. London: Springer London.
    q p q^{ -1 }
    -}
    rotate a (origin, onb, time) quat =
        let -- locs = [ locate v origin onb | v <- (toList $ localCoords a time) ]
            locs = toList $ localCoords a time
            rotatePoint point =
                let pquat = fromSVec2Quaternion 0.0 point
                    qinv = qInverse quat
                in hamiltonProduct qinv (hamiltonProduct pquat quat) 
            nlocs = map qVector $! map rotatePoint locs
        in transform a (NList (head nlocs) (tail nlocs))

    rotateByAxisAngle :: a -> LocatingParams -> AxisAngle -> a
    rotateByAxisAngle a loc (axis, angle) =
        let q = fromAngleAxis2Quaternion angle axis
            rq = rotate a loc q
        -- in error $ show rq ++ " " ++ show a
        in rq


class Scalable a where
    scale :: (Locatable a, Transformable a) => a -> Offset -> LocatingParams -> a
    scale a offset (origin, onb, time) =
        let locs = [ locate v origin onb | v <- (toList $ localCoords a time) ]
            nlocs = [multiply offset v | v <- locs]
        in transform a (NList (head nlocs) (tail nlocs))
