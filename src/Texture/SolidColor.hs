-- module solid color
module Texture.SolidColor where

-- math3d
import Math3D.Vector

-- color
import Color.ColorInterface
-- texture
import Texture.Texture

data SolidColor = SolidV Vector
                | SolidD Double Double Double

instance Texture SolidColor where
    color (SolidV a) _ _ _ = if (vsize a) == 3
                             then ColorInt { stype = RGB, colorData = a}
                             else fromPowers a

    color (SolidD a b c) _ _ _ = ColorInt {stype = RGB,
                                           colorData = fromList2Vec a [b, c]}

instance Eq SolidColor where
    (SolidV v) == (SolidD a b c) = v == (fromList2Vec a [b,c])
    (SolidV a) == (SolidV b ) = a == b
    (SolidD a b c) == (SolidV v) = v == (fromList2Vec a [b,c])
    (SolidD a b c) == (SolidD d e f) = (a == d) && (b == e) && (c == f)

instance Show SolidColor where
    show (SolidV a) = "Solid Color: " ++ show a
    show (SolidD a b c) = "Solid Color: " ++ (show $ fromList2Vec a [b, c])
