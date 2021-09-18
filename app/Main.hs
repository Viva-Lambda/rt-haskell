module Main where

import Lib
import ColorIO
import Color
import GHC.Float
import Vector
import Ray
import Prelude hiding(subtract)

-- hit object

hitSphere :: Vector -> Double -> Ray -> Bool
hitSphere center radius (Rd {origin = ro, direction = rd}) =
    let distOrCent = subtract ro center
        discrimA = lengthSquared rd
        discrimB = 2.0 * (dot distOrCent rd)
        discrimC = (lengthSquared distOrCent) - (radius * radius)
        discriminant = (discrimB * discrimB) - (4.0 * discrimA * discrimC)
    in discriminant > 0

-- image related
imageWidth :: Int
imageHeight :: Int

imageWidth = 400
imageHeight = double2Int $ (int2Double imageWidth) / aspectRatio

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

mkImageColCoordinate :: Int -> [Int] -> [(Int, Int)]
mkImageColCoordinate _ [] = []
mkImageColCoordinate j (i:iis) =
    (j, i) : mkImageColCoordinate j iis

mkPixelCoordinates :: [Int] -> [Int] -> [(Int, Int)]
mkPixelCoordinates [] [] = []
mkPixelCoordinates [] _ = []
mkPixelCoordinates _ [] = []
mkPixelCoordinates (j:jjs) iis =
    mkImageColCoordinate j iis ++ mkPixelCoordinates jjs iis

mkPixelRay :: (Int,Int) -> Ray 
mkPixelRay (i,j) =
    let u = (int2Double i) / (int2Double (imageWidth - 1))
        v = (int2Double j) / (int2Double (imageHeight - 1))
        vvert = multiplyS cameraV v
        uhor = multiplyS cameraH u
        vvMinOr = subtract vvert cameraOrigin
        uhorPlusVv = add uhor vvMinOr
        llcPlusUHor = add lowerLeftCorner uhorPlusVv
        ray = Rd {origin = cameraOrigin, direction = llcPlusUHor}
    in ray


rayColor :: Ray -> Vector
rayColor Rd {origin = a, direction = b} =
    let ray = Rd {origin = a, direction = b}
        hsphere = hitSphere (VecFromList [0.0, 0.0, -1.0]) 0.5 ray
    in if hsphere
       then VecFromList [1.0, 0.0, 0.0]
       else let
                unitDirection = toUnit b
                (VecFromList vdata) = fromScalarToList unitDirection
                yval = vdata !! 1 -- access y value
                tval = (yval + 1.0) * 0.5
                oneMin = 1.0 - tval
                cval = multiplyS (VecFromList [1.0, 1.0, 1.0]) oneMin
                oval = multiplyS (VecFromList [0.5, 0.7, 1.0]) tval
            in add cval oval

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
        vhalf = divideS cameraV 2.0
        hhalf = divideS cameraH 2.0
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
        jjs = reverse [0..(imageHeight-1)]; 
        iis = [0..imageWidth-1];
        pixCoords = mkPixelCoordinates jjs iis;
        ps = mkPixels pixCoords;
        }
    printPixels ps

main :: IO ()
main = printColor
