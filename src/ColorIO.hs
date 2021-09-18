-- input output of colors
module ColorIO where

import GHC.Float
import Vector

data Pixel = Pix {
    x :: Int,
    y :: Int,
    color :: Vector
    }


