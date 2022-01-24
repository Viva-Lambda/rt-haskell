-- pixel
module Color.Pixel where

import Math3D.Vector

data Pixel = Pix {x :: Int, y :: Int,
                  color :: Vector} deriving (Eq, Show)
