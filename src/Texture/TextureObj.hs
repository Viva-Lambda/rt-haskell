{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- module grouping texture adts
module Texture.TextureObj where

import Math3D.Vector

-- texture
import Texture.SolidColor
import Texture.Checker
import Texture.Noise
import Texture.Texture
import Texture.Image
import Texture.Spectral

data TextureObj where 
    TextureCons :: Texture a => a -> TextureObj

instance Texture TextureObj where
    color b hu hv hp w =
        case b of
            (TextureCons a) -> color a hu hv hp w
