-- basic enumeration types
module Utility.BaseEnum where


-- spectral value types
type WaveVal = Float
type PowerVal = Double



-- cie trichromatic
data CIETrichroma = CIE_X
                  | CIE_Y
                  | CIE_Z
                  deriving(Eq, Show)

-- rgb to spectral flags
data Rgb2Spect = REFL_WHITE
               | REFL_CYAN
               | REFL_MAGENTA
               | REFL_YELLOW
               | REFL_RED
               | REFL_GREEN
               | REFL_BLUE
               | ILLUM_WHITE
               | ILLUM_CYAN
               | ILLUM_MAGENTA
               | ILLUM_YELLOW
               | ILLUM_RED
               | ILLUM_GREEN
               | ILLUM_BLUE
               deriving (Show, Eq)

--
