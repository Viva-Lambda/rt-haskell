module Main where

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
import Material.Material
import Prelude hiding(subtract)
import Data.Time.Clock
import System.IO

-- image related

imageWidth :: Int
imageHeight :: Int

imageWidth = 400
imageHeight = double2Int $ (int2Double imageWidth) / aspectRatio

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

sample_per_pixel :: Int
sample_per_pixel = 100

bounceDepth :: Int
bounceDepth = 50

-- world

world :: HittableList

world = HList [
    -- center
    (HitSphere (SphereObj {sphereCenter = VList [0.0, 0.0, -1.0], 
                           sphereRadius = 0.5,
                           sphereMat = LambMat $ Lamb {lalbedo = VList [0.7, 0.3, 0.3]}
                           })),
    -- ground
    (HitSphere (SphereObj {sphereCenter = VList [0.0, -100.5, -1.0], 
                           sphereRadius = 100,
                           sphereMat = LambMat $ Lamb {lalbedo = VList [0.8, 0.8, 0.0]}
                           })),
    -- left
    (HitSphere (SphereObj {sphereCenter = VList [-1.0, 0.0, -1.0], 
                           sphereRadius = 0.5,
                           sphereMat = DielMat $ Diel {
                               refIndices = [1.5]
                               }
                           })),
    (HitSphere (SphereObj {sphereCenter = VList [-1.0, 0.0, -1.0], 
                           sphereRadius = -0.4,
                           sphereMat = DielMat $ Diel {
                               refIndices = [1.5]
                               }
                           })),
    -- right
    (HitSphere (SphereObj {sphereCenter = VList [1.0, 0.0, -1.0],
                           sphereRadius = 0.5,
                           sphereMat = MetalMat $ Met {
                               malbedo = VList [0.8, 0.6, 0.2],
                               fuzz = 1.0
                               }
                           }))
    ]


-- camera related

mkPixelRay :: RandomGen g => (Int, Int) -> g -> Camera -> (Ray, g)
mkPixelRay (j,i) gen cm =
    let (udouble, g1) = rand gen
        (vdouble, g2) = rand g1
        u = (udouble + (int2Double i)) / (int2Double (imageWidth - 1))
        v = (vdouble + (int2Double j)) / (int2Double (imageHeight - 1))
    in getRay g2 cm u v

-- rendering ppm related
printPPMHeader :: IO ()
printPPMHeader = do
    putStrLn "P3"
    putStrLn $ show imageWidth ++ " " ++ show imageHeight
    putStrLn "255"

-- make pixel colors from pixel coordinates

mkPixelColor :: RandomGen g => (Int, Int) -> g -> Camera -> Vector
mkPixelColor a g cm = 
    let -- (g1, g2) = split g
        (ray, g2) = mkPixelRay a g cm
    in rayColor ray world bounceDepth g2

foldPixelColors :: RandomGen g => (Int, Int) -> g -> Camera -> Vector
foldPixelColors a gen cm =
    let gens = randomGens gen sample_per_pixel
        pcolors = [mkPixelColor a g cm | g <- gens]
    in foldl1 add pcolors

mkPixels :: RandomGen g => [(Int, Int)] -> g -> Camera -> [Pixel]
mkPixels [] _ _ = []
mkPixels ((cy, cx):cs) g cm =
    let --(g1, g2) = split g
        p = Pix {x = cx, y = cy, color = foldPixelColors (cy, cx) g cm}
    in p : mkPixels cs g cm

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
    tstart <- getCurrentTime
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
    _ <- printPixels ps
    tend <- getCurrentTime
    let {diff = diffUTCTime tend tstart;
         secs = diff
        }
    hPutStrLn stderr ("duration in seconds: " ++ show secs )


main :: IO ()
main = printColor
