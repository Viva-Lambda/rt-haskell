-- colorable type
module Color.Colorable where

import Math3D.Vector

--
class Colorable a where
    toXYZ :: a -> Vector
    toRGB :: a -> Vector
