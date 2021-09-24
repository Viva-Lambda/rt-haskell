module Main where

import Lib
import ColorIO
import Pixel
import Color
import GHC.Float
import Vector
import Ray
import Hittable.HittableList
import Hittable.Sphere
import Prelude hiding(subtract)

-- image related

imageWidth :: Int
imageHeight :: Int

imageWidth = 400
imageHeight = double2Int $ (int2Double imageWidth) / aspectRatio

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

-- world

world :: HittableList

world = HList [
    (HitSphere (SphereObj {sphereCenter = VecFromList [0.0, 0.0, -1.0], 
                           sphereRadius = 0.5})),
    (HitSphere (SphereObj {sphereCenter = VecFromList [0.0, -100.5, -1.0], 
                           sphereRadius = 100}))
    ]


-- camera related
viewPortH :: Double
viewPortH = 2.0
viewPortW :: Double
viewPortW = aspectRatio * viewPortH

focalLength :: Double
focalLength = 1.0

cameraOrigin :: Vector
cameraOrigin = VecFromList [0.0, 0.0, 0.0]
cameraH :: Vector
cameraH = VecFromList [viewPortW, 0.0, 0.0]

cameraV :: Vector
cameraV = VecFromList [ 0.0, viewPortH, 0.0]

lowerLeftCorner :: Vector
lowerLeftCorner = 
    let fvec = VecFromList [0.0, 0.0, focalLength]
        vhalf = divideS cameraV 2.0
        hhalf = divideS cameraH 2.0
        origMinH = subtract cameraOrigin hhalf
        origMinHMinV = subtract origMinH vhalf
    in subtract origMinHMinV fvec


-- rendering ppm related
printPPMHeader :: IO ()
printPPMHeader = do
    putStrLn "P3"
    putStrLn $ show imageWidth ++ " " ++ show imageHeight
    putStrLn "255"

-- make pixel colors from pixel coordinates

mkPixelRay :: (Int,Int) -> Ray 
mkPixelRay (j,i) =
    let u = (int2Double i) / (int2Double (imageWidth - 1))
        v = (int2Double j) / (int2Double (imageHeight - 1))
        vvert = multiplyS cameraV v
        uhor = multiplyS cameraH u
        vvMinOr = subtract vvert cameraOrigin
        uhorPlusVv = add uhor vvMinOr
        llcPlusUHor = add lowerLeftCorner uhorPlusVv
        ray = Rd {origin = cameraOrigin, direction = llcPlusUHor}
    in ray


mkPixelColor :: (Int, Int) -> Vector
mkPixelColor a = rayColor ( mkPixelRay a) world

mkPixels :: [(Int, Int)] -> [Pixel]
mkPixels [] = []
mkPixels ((cy, cx):cs) =
    Pix {x = cx, y = cy, color = mkPixelColor (cy, cx)} : mkPixels cs

printPixel :: Pixel -> IO ()
printPixel (Pix {x = _, y = _, color = cs}) =
    let cstring = vecAsColor cs
    in putStrLn cstring

printPixels :: [Pixel] -> IO ()
printPixels [] = return ()
printPixels (p:ps) = do
                        _ <- printPixel p
                        printPixels ps

printColor :: IO ()
printColor = do
    _ <- printPPMHeader
    let {
        jjs = reverse [0..(imageHeight-1)]; 
        iis = [0..(imageWidth-1)];
        pixCoords = [(j,i) | j <- jjs, -- outer loop first
                             i <- iis];
        ps = mkPixels pixCoords;
        }
    -- print pixCoords
    printPixels ps

main :: IO ()
main = printColor
