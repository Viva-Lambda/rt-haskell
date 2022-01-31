-- module solid color
module Texture.SolidColor where

-- math3d
import Math3D.Vector

-- color
import Color.ColorInterface

-- spectral
import Spectral.SampledSpectrum
-- texture
import Texture.Texture

data SolidColor = SolidP Double
                | SolidD Double Double Double

instance Texture SolidColor where
    color (SolidP a) _ _ _ w = ColorRec { model = ColorSpec (ILLUMINANT, (w, a))}

    color (SolidD a b c) _ _ _ _ = ColorRec {model = ColorRGB $! fromList2Vec a [b, c]}

instance Eq SolidColor where
    (SolidP v) == (SolidD a b c) = False
    (SolidP a) == (SolidP b ) = a == b
    (SolidD a b c) == (SolidP v) = False
    (SolidD a b c) == (SolidD d e f) = (a == d) && (b == e) && (c == f)

instance Show SolidColor where
    show (SolidP a) = "Solid Color Power: " ++ show a
    show (SolidD a b c) = "Solid Color Vector: " ++ (show $ fromList2Vec a [b, c])
