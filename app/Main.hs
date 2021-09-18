module Main where

import Lib
import ColorIO
import Color
import GHC.Float
import Vector
import Ray
import Prelude hiding(subtract)

-- image related
imageWidth :: Int
imageHeight :: Int

imageWidth = 400
imageHeight = double2Int $ (int2Double imageWidth) / aspectRatio

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

mkImageColCoordinate :: Int -> [Int] -> [(Int, Int)]
mkImageColCoordinate _ [] = []
mkImageColCoordinate col (r:rows) =
    (col, r) : mkImageColCoordinate col rows

mkPixelCoordinates :: [Int] -> [Int] -> [(Int, Int)]
mkPixelCoordinates [] [] = []
mkPixelCoordinates [] _ = []
mkPixelCoordinates _ [] = []
mkPixelCoordinates (c:cs) rows =
    mkImageColCoordinate c rows ++ mkPixelCoordinates cs rows

mkPixelRay :: (Int,Int) -> Ray 
mkPixelRay (xc,yc) =
    let u = (int2Double xc) / (int2Double (imageWidth -1))
        v = (int2Double yc) / (int2Double (imageHeight - 1))
        vvert = multiply (VecFromScalar v (vsize cameraV)) cameraV
        uhor = multiply (VecFromScalar u (vsize cameraH)) cameraH
        vvMinOr = subtract vvert cameraOrigin
        uhorPlusVv = add uhor vvMinOr
        llcPlusUHor = add lowerLeftCorner uhorPlusVv
        ray = Rd {origin = cameraOrigin, direction = llcPlusUHor}
    in ray


rayColor :: Ray -> Vector
rayColor Rd {origin = _, direction = b} =
    let unitDirection = toUnit b
        (VecFromList vdata) = fromScalarToList unitDirection
        yval = vdata !! 1 -- access y value
        tval = (yval + 1.0) * 0.5
        cval = VecFromList [(1.0 - tval),(1.0 - tval),(1.0 - tval)]
        oval = VecFromList [0.5, 0.7, 1.0]
        other = multiply (VecFromScalar tval (vsize cval)) oval 
    in add cval other

mkPixelColor :: (Int, Int) -> Vector

mkPixelColor a = rayColor $ mkPixelRay a

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
cameraV = VecFromList [ 0.0,viewPortH, 0.0]

lowerLeftCorner :: Vector
lowerLeftCorner = 
    let fvec = VecFromList [0.0, 0.0, focalLength]
        vhalf = divide cameraV (VecFromScalar 2.0 (vsize cameraV))
        hhalf = divide cameraH (VecFromScalar 2.0 (vsize cameraH))
        vMinF = subtract vhalf fvec
        hMinV = subtract hhalf vMinF
        origMinH = subtract cameraOrigin hMinV
    in origMinH


-- rendering ppm related
printPPMHeader :: IO ()
printPPMHeader = do
    putStrLn "P3"
    putStrLn $ show imageWidth ++ " " ++ show imageHeight
    putStrLn "255"


printCList :: Show a => [a] -> IO ()
printCList lst = putStrLn $ unwords $ map show lst

pixColorToInt :: [Double] -> [Int]
pixColorToInt d = map double2Int d

-- make pixel colors from pixel coordinates
mkPixels :: [(Int, Int)] -> [Pixel]
mkPixels [] = []
mkPixels ((cx, cy):cs) =
    Pix {x = cx, y = cy, color = mkPixelColor (cx, cy)} : mkPixels cs

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
        invrows = reverse [0..(imageHeight-1)]; 
        cols = [0..(imageWidth)];
        pixCoords = mkPixelCoordinates cols invrows;
        ps = mkPixels pixCoords;
        }
    printPixels ps

main :: IO ()
main = printColor
