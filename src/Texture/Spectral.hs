-- spectral texture type: SampledSpectrum that supports texture calls
module Texture.Spectral where

import Spectral.SampledSpectrum

import Texture.Texture

data SpectralTexture = SpectT SampledSpectrum
