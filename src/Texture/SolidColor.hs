-- module solid color
module Texture.SolidColor where

import Math3D.Vector
import Texture.Texture

data SolidColor = SolidV Vector
                | SolidD Double Double Double

instance Texture SolidColor where
    color (SolidV a) _ _ _ = a
    color (SolidD a b c) _ _ _ = fromList2Vec a [b, c]

instance Eq SolidColor where
    (SolidV v) == (SolidD a b c) = v == (fromList2Vec a [b,c])
    (SolidV a) == (SolidV b ) = a == b
    (SolidD a b c) == (SolidV v) = v == (fromList2Vec a [b,c])
    (SolidD a b c) == (SolidD d e f) = (a == d) && (b == e) && (c == f)

instance Show SolidColor where
    show (SolidV a) = "Solid Color: " ++ show a
    show (SolidD a b c) = "Solid Color: " ++ (show $ fromList2Vec a [b, c])
