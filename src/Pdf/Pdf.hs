-- pdf handling code
module Pdf.Pdf where

-- math
import Math3D.Vector
import Math3D.Onb

-- random
import Random
import System.Random

class Pdf a where
    -- obtain pdf sampling value
    pvalue :: a -> Vector -> Double
    -- generate direction
    generate :: RandomGen g => a -> g -> (Vector, g)
