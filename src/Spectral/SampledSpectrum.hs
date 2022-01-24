-- sampled spectrum
module Spectral.SampledSpectrum where

import Spectral.SampledDistribution
import Spectral.PbrSpectrum

-- vector
import Math3D.Vector

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

-- fromRGB :: Double -> Double -> Double -> SpectrumType -> SampledSpectrum
-- fromRGB r g b t =
