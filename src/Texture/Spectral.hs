-- spectral texture type: SampledSpectrum that supports texture calls
module Texture.Spectral where

import Spectral.SampledSpectrum
import Spectral.SampledDistribution

import Texture.Texture

import Color.ColorInterface

data SpectralTexture = SpectT SampledSpectrum deriving (Eq, Show)

instance Texture SpectralTexture where
    color (SpectT s)  _ _ _ w = 
        let p = evaluateWave w $! sampled s
        in ColorRec {model = ColorSpec (spectrumType s, (w, p))}
