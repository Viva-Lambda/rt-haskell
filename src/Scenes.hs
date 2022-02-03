{-# LANGUAGE BangPatterns #-}
-- scene module
module Scenes where

import Data.Bitmap.Base
import System.Random

-- basic scene defaults
import Scene.Scene

-- diffuse sphere
import Scene.DiffuseSphere

-- random one weekend final
import Scene.RandomOneWeekendFinal

-- two spheres for checkered texture
import Scene.CheckerScene

-- two perlin spheres
import Scene.PerlinSphere

-- image texture
import Scene.ImageScene

-- simple light scene
import Scene.PerlinLight

-- cornell box
import Scene.CornellBox

-- cornell smoke
import Scene.CornellSmoke

-- next week final scene
import Scene.NextWeekFinal

-- cornell sphere
import Scene.CornellSphere

-- spectral checker sphere
import Scene.SpectralScene

chooseScene :: RandomGen g => g -> [Bitmap Word8] -> Int -> (Int, Scene)
chooseScene g !s choice =
    case choice of
        0 -> (nb_samples diffuseSphere, diffuseSphere)
        1 -> let sc = randomOneWeekendFinalSceneStatic g in (nb_samples sc, sc) 
        2 -> let sc = randomOneWeekendFinalSceneMove g in (nb_samples sc, sc)
        3 -> let sc = twoCheckeredSpheres in (nb_samples sc, sc)
        4 -> let sc = twoPerlinSpheres g in (nb_samples sc, sc)
        5 -> let sc = if null s
                      then diffuseSphere
                      else imgEarth (head s)
             in (nb_samples sc, sc)
        6 -> let sc = simpleLight g in (nb_samples sc, sc)
        7 -> let sc = cornellBox g in (nb_samples sc, sc)
        8 -> let sc = cornellSmoke g in (nb_samples sc, sc)
        9 -> let sc = if null s
                      then diffuseSphere
                      else nextWeekFinal g (head s)
             in (nb_samples sc, sc)
        10 -> let sc = cornellSphere g in (nb_samples sc, sc)
        11 -> let sc = cornellBoxSpectral g in (nb_samples sc, sc)
        _ -> let sc = diffuseSphere in (nb_samples sc, sc)

