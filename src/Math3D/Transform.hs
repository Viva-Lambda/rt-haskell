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
locate avec origin onb = add origin (localVec onb avec)

class Locatable a where
    localCoords :: a -> Double -> NonEmptyList Vector
    
    located :: Transformable a => a -> LocatingParams -> a
    located a (origin, onb, time) =
        let locs = [ locate v origin onb | v <- (toList $ localCoords a time) ]
        in transform a (NList (head locs) (tail locs))


class Translatable a where
    translate :: (Locatable a, Transformable a) => a -> Offset -> LocatingParams -> a
    translate a offset (origin, onb, time) =
        let locs = [ locate v origin onb | v <- (toList $ localCoords a time) ]
            nlocs = [ add v offset | v <- locs ]
        in transform a (NList (head nlocs) (tail nlocs))


class Rotatable a where
    rotate :: (Locatable a, Transformable a) => a -> LocatingParams -> Quaternion -> a
    {-
    Rotate a point with a quaternion, from:
    Vince, J. (2011) Quaternions for Computer Graphics. London: Springer London.
    q p q^{ -1 }
    -}
    rotate a (origin, onb, time) quat =
        let locs = [ locate v origin onb | v <- (toList $ localCoords a time) ]
            rotatePoint point =
                let pquat = fromSVec2Quaternion 0.0 point
                    qinv = qInverse quat
                in hamiltonProduct (hamiltonProduct quat pquat) qinv
            nlocs = map qVector $! map rotatePoint locs
        in transform a (NList (head nlocs) (tail nlocs))

    rotateByAxisAngle :: (Locatable a, Transformable a) => a -> LocatingParams -> AxisAngle -> a
    rotateByAxisAngle a loc (axis, angle) =
        let q = fromAngleAxis2Quaternion angle axis
        in rotate a loc q


class Scalable a where
    scale :: (Locatable a, Transformable a) => a -> Offset -> LocatingParams -> a
    scale a offset (origin, onb, time) =
        let locs = [ locate v origin onb | v <- (toList $ localCoords a time) ]
            nlocs = [multiply offset v | v <- locs]
        in transform a (NList (head nlocs) (tail nlocs))
