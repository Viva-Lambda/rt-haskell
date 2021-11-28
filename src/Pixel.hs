-- pixel
module Pixel where

import Vector

data Pixel = Pix {x :: Int, y :: Int,
                  color :: Vector} deriving (Eq, Show)
