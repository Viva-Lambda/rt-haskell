{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- constant medium module
module Hittable.ConstantMedium where

-- math
import Math3D.Vector
import Math3D.CommonOps
import Math3D.Ray

-- hittable
import Hittable.Hittable
import Hittable.HitRecord
import Hittable.Aabb

-- material
import Material.Material

-- texture
import Texture.Texture
import Texture.TextureObj

-- utility
import Utility.Utils

-- other stuff
import Prelude hiding(subtract)

data ConstantMedium where
    ConsMedium :: Hittable a => a -> Material -> Double -> ConstantMedium

--
{-
mkConstantMedium :: (Hittable a, Texture b) => a -> Double -> b -> ConstantMedium
mkConstantMedium boundary density tex =
    ConsMedium boundary (IsotMat $ Isot {isalbedo = TextureCons tex}) ((-1.0) / density)
    -}
