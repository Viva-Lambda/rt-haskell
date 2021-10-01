{-# LANGUAGE BangPatterns #-}
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
import Prelude hiding(subtract)
import Data.Time.Clock
import System.IO
import Scenes

-- image related

imageWidth :: Int
imageHeight :: Int

imageWidth = 320
imageHeight = double2Int $! (int2Double imageWidth) / aspectRatio

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

sample_per_pixel :: Int
sample_per_pixel = 50

bounceDepth :: Int
bounceDepth = 20

-- world


-- camera related

mkPixelRay :: RandomGen g => (Int, Int) -> g -> Camera -> (Ray, g)
mkPixelRay !(j,i) !gen !cm =
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

renderScene :: RandomGen g => [(Int, Int)] -> g -> Scene -> [Pixel]
renderScene !cs !g scn =
    let cmfrom = cam_look_from scn
        cmto = cam_look_to scn
        cmvf = cam_vfov scn
        cmvup = cam_vup scn
        cm_fdist = cam_focus_distance scn
        cm_apr = cam_aperture scn
        sample_pixs = nb_samples scn
        aratio = aspect_ratio scn
        bdepth = bounce_depth scn
        cam = mkCam cmfrom cmto  cmvup cmvf aratio cm_apr cm_fdist 0.0 0.0
        wrld = scene_obj scn
    in pixels g cs sample_pixs cam wrld bdepth
    where pixels gen ((cy, cx):cc) nb_smpl cMra objs depth =
            let p = Pix {x = cx, y = cy,
                         color = foldColor gen (cy, cx) nb_smpl cMra objs depth}
            in p : pixels gen cc nb_smpl cMra objs depth
          pixels _ [] _ _ _ _ = []

          foldColor rng coord nsmp cmra sobjs bd =
            let rngs = randomGens rng nsmp
                pcols = [mkColor coord gn cmra sobjs bd | gn <- rngs] 
            in foldl1 add pcols

          mkColor coord rng cmr sobjs bdepth =
            let (ray, g2) = mkPixelRay coord rng cmr
            in rayColor ray sobjs bdepth g2

printPixel :: Pixel -> Int -> IO ()
printPixel !(Pix {x = _, y = _, color = cs}) !smpl =
    let cstring = writeColor cs smpl
    in putStrLn cstring

printPixels :: [Pixel] -> Int -> IO ()
printPixels ![] _ = return ()
printPixels !(p:ps) nbsmp = do
                        _ <- printPixel p nbsmp
                        printPixels ps nbsmp

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
        -- choose scene
        (smpl, scne) = chooseScene g 1;
        ps = renderScene pixCoords g scne;
        }
    -- print pixCoords
    _ <- printPixels ps smpl
    tend <- getCurrentTime
    let {diff = diffUTCTime tend tstart;
         secs = diff
        }
    hPutStrLn stderr ("duration in seconds: " ++ show secs )


main :: IO ()
main = printColor
