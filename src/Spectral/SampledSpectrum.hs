-- sampled spectrum
module Spectral.SampledSpectrum where

import Math3D.Vector
import Spectral.SampledDistribution

data SpectrumType = REFLECTANCE
                  | ILLUMINANT
                  | RGB
                  deriving (Show, Eq)

data SampledSpectrum = SSpec {
    -- Spectrum Type
    spectrumType :: SpectrumType,
    -- sampled data
    sampled :: SampledWavePower
    } deriving (Eq, Show)
