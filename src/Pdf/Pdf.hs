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
    pvalue :: RandomGen g => a -> g -> Vector -> (Double, g)
    -- generate direction
    generate :: RandomGen g => a -> g -> (Vector, g)
