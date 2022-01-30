-- pixel
module Color.Pixel where

import Math3D.Vector

import Color.ColorInterface
import Spectral.SampledSpectrum

data PixelSpectrum = PixSpecTrichroma (Double, Double, Double)
                   | PixSpecSampled SampledSpectrum
                   deriving (Eq, Show)

data Pixel = Pix {x :: Int, y :: Int,
                  color :: PixelSpectrum} deriving (Eq, Show)
