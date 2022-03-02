-- module containing wave spectrum, that is a list of wave
module Physics.WaveSpectrum where

import Physics.Wave

import Utility.HelperTypes

data WaveSpectrum = WaveSpect (NonEmptyList PlaneWave)

