{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- Checkered texture
module Texture.Checker where

import Math3D.Vector
import Texture.Texture

data Checker where
    CheckT :: forall a b. (Eq a, Eq b, Texture a, Texture b) => a -> b -> Checker


instance Texture Checker where
    color (CheckT a b) hu hv hp w = 
        let xv = vget hp 0
            yv = vget hp 1
            zv = vget hp 2
            sinval = sin xv * sin yv * sin zv
            cval = if sinval < 0
                   then color a hu hv hp w
                   else color b hu hv hp w
        in cval
