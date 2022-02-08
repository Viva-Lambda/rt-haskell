{-# LANGUAGE BangPatterns #-}
module Main where
-- stack install --profile --local-bin-path ./bin/
-- options +RTS (enables statistics) -N2 (two threads) -p 
-- the order of options is important

import Render
import Color.Pixel
import Color.ColorIO
import System.Random
import Prelude hiding(subtract)
import Data.Time.Clock
import System.IO
import Codec.Image.STB

import Scene.Scene

import Scenes

-- world

-- camera related

-- rendering ppm related
printPPMHeader :: Int -> Int -> IO ()
printPPMHeader imw imh = do
    putStrLn "P3"
    putStrLn $ show imw ++ " " ++ show imh
    putStrLn "255"

-- make pixel colors from pixel coordinates

printPixel :: Pixel -> Int -> IO ()
printPixel !(Pix {x = _, y = _, color = cs}) !smpl =
    let cstring = writeColor cs smpl
    in putStrLn cstring

printPixels :: [Pixel] -> Int -> IO ()
printPixels ![] _ = return ()
printPixels !(p:ps) nbsmp = do
                        _ <- printPixel p nbsmp
                        printPixels ps nbsmp

type ImLoad = Either String Image

-- sceneChoice
-- 0: diffuse, 1: oneweekend final
-- 2: oneweekend final with motion blur
-- 3: checkered sphere
-- 4: perlin noise sphere
-- 5: image texture

traceScene :: RandomGen g => g-> MayImage -> Int -> IO (Int, (Int, Int), [Pixel])
traceScene g imD sceneChoice =
    let imval = case imD of
                    Nothing -> []
                    Just e -> [e]
        (smpl, scne) = chooseScene g imval sceneChoice
        imw = img_width scne
        imh = img_height scne
        jjs = reverse [0..(imh-1)]
        iis = [0..(imw-1)]
        pixCoords = [(j,i) | j <- jjs, -- outer loop first
                             i <- iis]
        ps = renderScene pixCoords g scne
    in return (smpl, (imw, imh), ps)

type MayImage = Maybe Image

loadImages :: Int -> [String] -> IO MayImage
loadImages choice paths =
    let choiceResult = case choice of
                            -- earth scene
                            5 -> loadImage (head paths)
                            -- nextweek final scene
                            9 -> loadImage (head paths);
                            -- demotic cornell box scene
                            12 -> loadImage (last paths); 
                            _ -> return (Left "no image");
    in do 
        ioResult <- choiceResult;
        case ioResult of
            Right a -> return (Just a)
            Left _ -> return Nothing


printColor :: IO ()
printColor = do
    tstart <- getCurrentTime
    g <- newStdGen
    choice <- return 12
    imD <- loadImages choice ["./earthmap.jpg", "./demotic.jpg"]
    (smpl, (imw, imh), ps) <- traceScene g imD choice
    -- print pixCoords
    _ <- printPPMHeader imw imh
    _ <- printPixels ps smpl
    tend <- getCurrentTime
    let {diff = diffUTCTime tend tstart;
         secs = diff
        }
    hPutStrLn stderr ("duration in seconds: " ++ show secs )



main :: IO ()
main = printColor
