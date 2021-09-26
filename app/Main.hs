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

mkRndMat :: RandomGen g => g -> Int -> Int -> Maybe HittableObj
mkRndMat gen a b =
    let (chooseMat, g1) = rand gen
        (cxrand, g2) = rand g1
        (czrand, g3) = rand g2
        center = VList [
            (int2Double a) + (0.9 * cxrand),
            0.2,
            (int2Double b) + (0.9 * czrand)
            ]
        cdiff = magnitude $! subtract center (VList [4.0, 0.2, 0.0])

    in if cdiff > 0.9
       then if chooseMat < 0.8
            then let (rv1, g4) = randV g3
                     (rv2, _) = randV g4
                     diffAlbedo = multiply rv1 rv2
                     laMat = LambMat $! Lamb {lalbedo = diffAlbedo}
                in Just $!HitSphere SphereObj {sphereCenter = center,
                                      sphereRadius = 0.2,
                                      sphereMat = laMat}
            else if chooseMat < 0.9
                 then let (rv1, g4) = randomVec (0.5, 1.0) g3
                          (fz, _) = randomDouble g4 0.0 0.5
                          metMat = MetalMat $! Met {malbedo = rv1, fuzz = fz}
                      in Just $!HitSphere SphereObj {
                                    sphereCenter = center,
                                    sphereRadius = 0.2,
                                    sphereMat = metMat
                                    }
                 else let dieMt = DielMat $! Diel {refIndices = [1.5]}
                      in Just $! HitSphere SphereObj {
                                    sphereCenter = center,
                                    sphereRadius = 0.2,
                                    sphereMat = dieMt
                                }
       else Nothing

mkRndMats :: RandomGen g => g -> [(Int, Int)] -> [HittableObj]
mkRndMats _ [] = []
mkRndMats gen ((a, b):es) =  case mkRndMat gen a b of
                                Just c -> c : mkRndMats gen es
                                Nothing -> mkRndMats gen es

world :: RandomGen g => g -> HittableList
world gen = let as = reverse [(-11)..11]
                bs = [(-11)..11]
                coords = zip as bs
                objs = mkRndMats gen coords
                groundMat = LambMat $! Lamb {lalbedo = VList [0.5, 0.5, 0.5]}
                ground = HitSphere SphereObj {sphereCenter = VList [0.0, -1000.0, 0.0],
                                    sphereRadius = 1000.0,
                                    sphereMat = groundMat}
                dielM1 = DielMat $! Diel {refIndices = [1.5]}
                lambM2 = LambMat $! Lamb {lalbedo = VList [0.4, 0.2, 0.1]}
                metalM3 = MetalMat $! Met {
                                        malbedo = VList [0.7, 0.6, 0.5],
                                        fuzz = 0.0
                                    }
                dielObj = HitSphere SphereObj {
                            sphereCenter = VList [0.0, 1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = dielM1 
                        }
                lambObj =HitSphere  SphereObj {
                            sphereCenter = VList [-4.0, 1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = lambM2
                        }
                metObj = HitSphere SphereObj {
                            sphereCenter = VList [4.0, 1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = metalM3
                        }
            in HList ([ground] ++ objs ++ [dielObj, lambObj, metObj])


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
    in rayColor ray (world g2) bounceDepth g2

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
