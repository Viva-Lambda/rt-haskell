-- camera module
module Camera where

import Vector
import Ray
import Prelude hiding (subtract)


data Camera = Cam {
    corigin :: Vector,
    lowerLeftCorner :: Vector,
    horizontal :: Vector,
    vertical :: Vector
    } deriving (Eq, Show)

mkCamera :: Camera
mkCamera =
    let aspectRatio = 16.0 / 9.0
        viewPortH = 2.0
        viewPortW = aspectRatio * viewPortH
        focalLength = 1.0
        cameraOrigin = VecFromList [0.0, 0.0, 0.0]
        cameraH = VecFromList [viewPortW, 0.0, 0.0]
        cameraV = VecFromList [ 0.0, viewPortH, 0.0]
        llCorner = let fvec = VecFromList [0.0, 0.0, focalLength]
                       vhalf = divideS cameraV 2.0
                       hhalf = divideS cameraH 2.0
                       origMinH = subtract cameraOrigin hhalf
                       origMinHMinV = subtract origMinH vhalf
                   in subtract origMinHMinV fvec
    in Cam {
        corigin = cameraOrigin,
        horizontal = cameraH,
        vertical = cameraV,
        lowerLeftCorner = llCorner
        }

getRay :: Camera -> Double -> Double -> Ray
getRay Cam {corigin = cameraOrigin, horizontal = cameraH,
            vertical = cameraV, lowerLeftCorner = llCorner} u v =
    --
    let vvert = multiplyS cameraV v
        uhor = multiplyS cameraH u
        vvMinOr = subtract vvert cameraOrigin
        uhorPlusVv = add uhor vvMinOr
        llcPlusUHor = add llCorner uhorPlusVv
    in Rd {origin = cameraOrigin, direction = llcPlusUHor}

