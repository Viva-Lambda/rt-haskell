{-# LANGUAGE BangPatterns #-}
-- camera module
module Camera where

-- math
import Math3D.Vector
import Math3D.CommonOps
import Math3D.Ray
import Math3D.Transform
import Math3D.Onb

import Utility.Utils
import Prelude hiding (subtract)
import System.Random
import Random


data Camera = Cam {
    corigin :: Vector,
    lowerLeftCorner :: Vector,
    horizontal :: Vector,
    vertical :: Vector,
    camU :: Vector,
    camV :: Vector,
    camW :: Vector,
    lensRadius :: Double,
    time0 :: Double,
    time1 :: Double
    } deriving (Eq, Show)

mkCam :: Vector -> Vector -> Vector -> Double -> Double -> Double -> Double -> Double -> Double -> Camera
mkCam !lookFrom !lookAt !vup !vfov !aspect_ratio !aperture !focusDist !t0 !t1 =
    let theta = degrees_to_radians vfov
        h = tan (theta / 2.0)
        viewPortH = 2.0 * h
        viewPortW = aspect_ratio * viewPortH
        cw = toUnit $! subtract lookFrom lookAt
        cu = toUnit $! cross3d vup cw
        cv = cross3d cw cu
        cameraOrigin = lookFrom
        cameraH = multiplyS cu (focusDist * viewPortW)
        cameraV = multiplyS cv (focusDist * viewPortH)
        fDcw = multiplyS cw focusDist
        vhalf = divideS cameraV 2.0
        hhalf = divideS cameraH 2.0
        llC1 = subtract cameraOrigin hhalf
        llc2 = subtract llC1 vhalf
        llCorner = subtract llc2 fDcw
    in Cam {
        corigin = cameraOrigin,
        lowerLeftCorner = llCorner,
        horizontal = cameraH,
        vertical = cameraV,
        camU = cu,
        camW = cw,
        camV = cv,
        lensRadius = aperture / 2.0,
        time0 = t0,
        time1 = t1
        }

mkCamTime :: Vector -> Vector -> Vector -> Double -> Double -> Double -> Double -> Camera
mkCamTime lookFrom lookAt vup vfov aspect_ratio aperture focusDist =
    mkCam lookFrom lookAt vup vfov aspect_ratio aperture focusDist 0.0 0.0

--
getCameraOnb :: Camera -> OrthoNormalBase
getCameraOnb c =
    let w = camW c
        v = camV c
        u = camU c
    in onb3 w v u

getCameraTime :: RandomGen g => g -> Camera -> Double
getCameraTime gen c =
    let t0 = time0 c
        t1 = time1 c
        RandResult (res, _) = randomDouble gen (t0, t1)
    in res

getCameraLocatingParams :: RandomGen g => g -> Camera -> LocatingParams
getCameraLocatingParams g c = (corigin c, getCameraOnb c, getCameraTime g c)

-- camera for listing 69
lookF :: Vector
lookF = fromList2Vec 13.0 [2.0, 3.0]
lookT :: Vector
lookT = fromList2Vec 0.0 [0.0, 0.0]
vUp :: Vector
vUp = fromList2Vec 0.0 [1.0, 0.0]

mkCamera :: Camera
mkCamera =
    mkCamTime
        lookF -- look from
        lookT -- look to
        vUp -- vup
        20.0 -- vfov
        (3.0/2.0) -- aspect ratio
        0.1 -- aperture
        10.0 -- focus distance 

getRay :: RandomGen g => g -> Camera -> Double -> Double -> RandomResult Ray g
getRay gen Cam {corigin = cameraOrigin, horizontal = cameraH,
                vertical = cameraV, lowerLeftCorner = llCorner,
                camU = cu, camW = cw, camV = cv, lensRadius = lr,
                time0 = t0, time1 = t1
            } s t =
    let RandResult (uvec, g) = randomUnitDisk gen
        rd = multiplyS uvec lr
        rdx = vget rd 0
        rdy = vget rd 1
        offset = add (multiplyS cu rdx) (multiplyS cv rdy)
        rorigin = add cameraOrigin offset
        rdir1 = add llCorner (multiplyS cameraH s)
        rdir2 = add rdir1 (multiplyS cameraV t)
        rdir3 = subtract rdir2 cameraOrigin
        rdir4 = subtract rdir3 offset
        RandResult (timeD, g2) = randomDouble g (t0, t1)
        RandResult (wlength, g3) = randomWord g (visible_lambda_start, visible_lambda_end)
    in RandResult (Rd {origin = rorigin,
                       direction = rdir4,
                       rtime = timeD,
                       wavelength = wlength}, g3)
