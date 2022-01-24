{-# LANGUAGE BangPatterns #-}
-- input output of colors
module Color.ColorInterface where

import Color.Pixel

import Math3D.Vector

import Spectral.SampledSpectrum


data ColorInterface = ColorInt {
    stype :: SpectrumType,
    colorData :: Vector
    }

fromPowers :: Vector -> ColorInterface
fromPowers a = ColorInt {
        stype = ILLUMINANT,
        colorData = a
    }

fromRGB :: Double -> Double -> Double -> ColorInterface
fromRGB a b c = ColorInt {
        stype = RGB,
        colorData = fromList2Vec a [b, c]
    }
