module Main where

import Lib
import ColorIO
import Pixel
import Color
import GHC.Float
import System.Random
import Random
import Vector
import Ray
import Camera
import Hittable.HittableList
import Hittable.Sphere
import Prelude hiding(subtract, rand)

-- image related

imageWidth :: Int
imageHeight :: Int

imageWidth = 400
imageHeight = double2Int $ (int2Double imageWidth) / aspectRatio

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

sample_per_pixel :: Int
sample_per_pixel = 100

-- world

world :: HittableList

world = HList [
    (HitSphere (SphereObj {sphereCenter = VecFromList [0.0, 0.0, -1.0], 
                           sphereRadius = 0.5})),
    (HitSphere (SphereObj {sphereCenter = VecFromList [0.0, -100.5, -1.0], 
                           sphereRadius = 100}))
    ]


-- camera related

mkPixelRay :: RandomGen g => (Int, Int) -> g -> Camera -> Ray
mkPixelRay (j,i) gen cm =
    let u = ((rand gen) + (int2Double i)) / (int2Double (imageWidth - 1))
        v = ((rand gen) + (int2Double j)) / (int2Double (imageHeight - 1))
    in getRay cm u v

-- rendering ppm related
printPPMHeader :: IO ()
printPPMHeader = do
    putStrLn "P3"
    putStrLn $ show imageWidth ++ " " ++ show imageHeight
    putStrLn "255"

-- make pixel colors from pixel coordinates

mkPixelColor :: RandomGen g => (Int, Int) -> g -> Camera -> Vector
mkPixelColor a g cm = rayColor ( mkPixelRay a g cm) world

foldPixelColors :: RandomGen g => (Int, Int) -> g -> Camera -> Vector
foldPixelColors a gen cm =
    let startv = VecFromList [0.0, 0.0, 0.0]
        pcolors = [mkPixelColor a gen cm | _ <- [0..sample_per_pixel]]
    in foldl1 add pcolors

mkPixels :: RandomGen g => [(Int, Int)] -> g -> Camera -> [Pixel]
mkPixels [] _ _ = []
mkPixels ((cy, cx):cs) g cm =
    Pix {x = cx, y = cy, color = foldPixelColors (cy, cx) g cm} : mkPixels cs g cm

printPixel :: Pixel -> IO ()
printPixel (Pix {x = _, y = _, color = cs}) =
    let cstring = writeColor cs sample_per_pixel
    in putStrLn cstring

printPixels :: [Pixel] -> IO ()
printPixels [] = return ()
printPixels (p:ps) = do
                        _ <- printPixel p
                        printPixels ps

printColor :: IO ()
printColor = do
    _ <- printPPMHeader
    g <- newStdGen
    let {
        jjs = reverse [0..(imageHeight-1)]; 
        iis = [0..(imageWidth-1)];
        pixCoords = [(j,i) | j <- jjs, -- outer loop first
                             i <- iis];
        ps = mkPixels pixCoords g (mkCamera);
        }
    -- print pixCoords
    printPixels ps

main :: IO ()
main = printColor
