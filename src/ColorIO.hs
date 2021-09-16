-- input output of colors
module ColorIO where

import GHC.Float

data Pixel = Pix {
    x :: Int,
    y :: Int,
    color :: [Double]
    }

imageWidth :: Int
imageHeight :: Int

imageWidth = 256
imageHeight = 256

printPPMHeader :: IO ()
printPPMHeader = do
    putStrLn "P3"
    putStrLn $ show imageWidth ++ " " ++ show imageHeight
    putStrLn "255"

mkImageColCoordinate :: Int -> [Int] -> [(Int, Int)]
mkImageColCoordinate col [] = []
mkImageColCoordinate col (r:rows) =
    (col, r) : mkImageColCoordinate col rows

mkPixelCoordinates :: [Int] -> [Int] -> [(Int, Int)]
mkPixelCoordinates [] [] = []
mkPixelCoordinates [] _ = []
mkPixelCoordinates _ [] = []
mkPixelCoordinates (c:cs) rows =
    mkImageColCoordinate c rows ++ mkPixelCoordinates cs rows

mkPixelColor :: (Int,Int) -> [Double]
mkPixelColor (xc,yc) =
    let rval = (int2Double xc) / (int2Double imageWidth)
        gval = (int2Double yc) / (int2Double imageHeight)
        bval = 0.25
        ir =  rval * 255.9
        ig =  gval * 255.9
        ib =  bval * 255.9
    in [ir, ig, ib]

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
    let intColors = pixColorToInt cs
    in printCList intColors

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
