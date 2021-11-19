{-# LANGUAGE BangPatterns #-}
-- scene module
module Scenes where

import Hittable.HittableList
import Hittable.HittableObj
import Hittable.Sphere
import Hittable.MovingSphere
import Hittable.AaRect
import Texture.SolidColor
import Texture.Checker
import Texture.TextureObj
import Texture.Noise
import Texture.Image
import Data.Bitmap.Base
import Data.Bitmap.Simple
import System.Random
import Random
import Math3D.Vector
import Math3D.CommonOps
import Camera
import Math3D.Ray
import Material.Material
import GHC.Float
import Prelude hiding(subtract)

data Scene = SceneVals {
    img_width :: Int,
    img_height :: Int,
    aspect_ratio :: Double,
    nb_samples :: Int,
    bounce_depth :: Int,
    cam_look_from :: Vector,
    cam_look_to :: Vector,
    cam_vup :: Vector,
    cam_vfov :: Double,
    cam_aperture :: Double,
    cam_focus_distance :: Double,
    scene_obj :: HittableList,
    back_ground :: Vector
    }


mkRndMat :: RandomGen g => g -> Int -> Int -> Bool -> (Maybe HittableObj, g)
mkRndMat gen !a !b !isMoving =
    let (chooseMat, g1) = randval gen
        (cxrand, g2) = randval g1
        (czrand, g3) = randval g2
        center = VList [
            (int2Double a) + (0.9 * cxrand),
            0.2,
            (int2Double b) + (0.9 * czrand)
            ]
        diff = subtract center (VList [4.0, 0.2, 0.0])
        cdiff = magnitude diff
    in if cdiff > 0.9
       then if (chooseMat > 0.3) && (chooseMat < 0.8)
            then let (rv1, g4) = randV g3
                     (rv2, g5) = randV g4
                     diffAlbedo = multiply rv1 rv2
                     laMat = LambMat $! Lamb {lalbedo = SolidTexture $! SolidV diffAlbedo}
                in if isMoving
                   then let (rv3, g6) = randomDouble g5 0.0 0.5
                        in (Just $! MvHitSphere MovSphereObj {
                            msphereCenter1 = center,
                            msphereCenter2 = add center (VList [0.0, rv3, 0.0]),
                            msphereRadius = 0.2,
                            msphereMat = laMat,
                            mTime0 = 0.0,
                            mTime1 = 1.0
                            }, g6)
                   else (Just $! HitSphere SphereObj {sphereCenter = center,
                                                     sphereRadius = 0.2,
                                                     sphereMat = laMat}, g5)
            else if chooseMat >= 0.8 && chooseMat < 0.9
                 then let (rv1, g4) = randomVec (0.5, 1.0) g3
                          (fz, g5) = randomDouble g4 0.0 0.5
                          metMat = MetalMat $! Met {malbedo = SolidTexture $!(SolidV rv1), fuzz = fz}
                      in (Just $! HitSphere SphereObj {
                                    sphereCenter = center,
                                    sphereRadius = 0.2,
                                    sphereMat = metMat
                                    }, g5)
                 else let dieMt = DielMat $! Diel {refIndices = [1.5]}
                      in (Just $! HitSphere SphereObj {
                                    sphereCenter = center,
                                    sphereRadius = 0.2,
                                    sphereMat = dieMt
                                }, g3)
       else (Nothing, gen)

mkRndMats :: RandomGen g => g -> Bool -> [(Int, Int)] -> [HittableObj]
mkRndMats _ _ [] = []
mkRndMats gen !isMov !((a, b):es) = case mkRndMat gen a b isMov of
                                     (Just c, g) -> c : mkRndMats g isMov es
                                     (Nothing, g) -> mkRndMats g isMov es

world :: RandomGen g => g -> Bool -> HittableList
world gen !isM = let as = [0..7]
                     bs = [0..7]
                     coords = [(a - 3, b - 3) | a <- as, b <- bs]
                     objs = mkRndMats gen isM coords
                     groundMat = LambMat $! Lamb {lalbedo = SolidTexture $! SolidV ( VList [0.5, 0.5, 0.5])}
                     ground = HitSphere SphereObj {
                                    sphereCenter = VList [0.0, -1000.0, 0.0],
                                    sphereRadius = 1000.0,
                                    sphereMat = groundMat}
                     dielM1 = DielMat $! Diel {refIndices = [1.5]}
                     lambM2 = LambMat $! Lamb {lalbedo = SolidTexture $! SolidV ( VList [0.4, 0.2, 0.1])}
                     metalM3 = MetalMat $! Met {
                                        malbedo = SolidTexture $! SolidV ( VList [0.7, 0.6, 0.5] ),
                                        fuzz = 0.0
                                    }
                     dielObj = HitSphere $! SphereObj {
                            sphereCenter =VList [0.0, 1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = dielM1 
                        }
                     lambObj = HitSphere $! SphereObj {
                            sphereCenter = VList [-4.0, 1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = lambM2
                        }
                     metObj = HitSphere $! SphereObj {
                            sphereCenter =  VList [4.0, 1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = metalM3
                        }
                  in if null objs
                     then error $ unwords (map show objs)
                     else HList ( objs ++ [ground] ++ [dielObj, lambObj, metObj])


worldStat :: RandomGen g => g -> HittableList
worldStat g = world g False

worldMoving :: RandomGen g => g -> HittableList
worldMoving g = world g True

-- book scenes

-- default values
imageWidth :: Int
imageWidth = 320

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageHeight :: Int
imageHeight = double2Int $! (int2Double imageWidth) / aspectRatio

nbSamples :: Int
nbSamples = 50

bounceDepth :: Int
bounceDepth = 20

camLookFrom :: Vector
camLookFrom = VList [13.0, 2.0, 3.0]

camLookTo :: Vector
camLookTo = VList [0.0, 0.0, 0.0]

camVFov :: Double
camVFov = 20.0

camVUp :: Vector
camVUp = VList [0.0, 1.0, 0.0]

camFocDistance :: Double
camFocDistance = 10.0

diffuseSphere :: Scene
diffuseSphere =
    let sobj = HList [
            HitSphere $! SphereObj {
                            sphereCenter = VList [-4.0, 1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = LambMat $! Lamb {
                                    lalbedo =SolidTexture $! SolidV ( VList [0.4, 0.2, 0.1])
                                    }
                        },
            HitSphere $ SphereObj {
                            sphereCenter = VList [0.0, -1000.0, 0.0],
                            sphereRadius = 1000.0,
                            sphereMat =  LambMat $! Lamb {
                                    lalbedo =SolidTexture $! SolidV ( VList [0.5, 0.5, 0.5])
                                    }
                        }
            ]
    in SceneVals {
        img_width = imageWidth,
        aspect_ratio = aspectRatio,
        img_height = imageHeight,
        nb_samples = nbSamples,
        bounce_depth = bounceDepth,
        cam_look_from = camLookFrom,
        cam_look_to = camLookTo,
        cam_vfov = camVFov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.1,
        scene_obj = sobj,
        back_ground = VList [0.7, 0.8, 1.0]
         }

randomOneWeekendFinalScene :: RandomGen g => g -> Bool -> Scene
randomOneWeekendFinalScene g b =
    let hl = world g b
    in SceneVals {
        img_width = imageWidth,
        aspect_ratio = aspectRatio,
        img_height = imageHeight,
        nb_samples = nbSamples,
        bounce_depth = bounceDepth,
        cam_look_from = camLookFrom,
        cam_look_to = camLookTo,
        cam_vfov = camVFov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.1,
        scene_obj = hl,
        back_ground = VList [0.7, 0.8, 1.0]
    }

randomOneWeekendFinalSceneStatic ::RandomGen g => g -> Scene
randomOneWeekendFinalSceneStatic g = randomOneWeekendFinalScene g False

randomOneWeekendFinalSceneMove ::RandomGen g => g -> Scene
randomOneWeekendFinalSceneMove g = randomOneWeekendFinalScene g True

-- two spheres for checkered texture

twoCheckeredSpheres :: Scene
twoCheckeredSpheres =
    let s1 = SolidV $! VList [0.2, 0.3, 0.1]
        s2 = SolidV $! VList [0.9, 0.9, 0.8]
        tobj = CheckerTexture $! CheckT s1 s2
        lmb = LambMat $! Lamb {lalbedo = tobj}
        sp1 = SphereObj {sphereCenter = VList [0.0, -10.0, 0.0],
                         sphereRadius = 10,
                         sphereMat = lmb}
        sp2 = SphereObj {sphereCenter = VList [0.0, 10.0, 0.0],
                         sphereRadius = 10,
                         sphereMat = lmb}
        hs = HList [HitSphere sp1, HitSphere sp2]
    in SceneVals {
        img_width = imageWidth,
        aspect_ratio = aspectRatio,
        img_height = imageHeight,
        nb_samples = nbSamples,
        bounce_depth = bounceDepth,
        cam_look_from = camLookFrom,
        cam_look_to = camLookTo,
        cam_vfov = camVFov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        back_ground = VList [0.7, 0.8, 1.0]
    }

-- two perlin spheres

twoPerlinSpheres :: RandomGen g => g -> Scene
twoPerlinSpheres g =
    let ptex = NoiseTexture $! mkPerlinNoise g 4.0
        lmb = LambMat $! Lamb {lalbedo = ptex}
        sp1 = SphereObj {sphereCenter = VList [0.0, -1000.0, 0.0],
                         sphereRadius = 1000,
                         sphereMat = lmb}
        sp2 = SphereObj {sphereCenter = VList [0.0, 2.0, 0.0],
                         sphereRadius = 2,
                         sphereMat = lmb}
        hs = HList [HitSphere sp1, HitSphere sp2]
    in SceneVals {
        img_width = imageWidth,
        aspect_ratio = aspectRatio,
        img_height = imageHeight,
        nb_samples = nbSamples,
        bounce_depth = bounceDepth,
        cam_look_from = camLookFrom,
        cam_look_to = camLookTo,
        cam_vfov = camVFov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        back_ground = VList [0.7, 0.8, 1.0]
    }

-- image texture
imgEarth :: Bitmap Word8 -> Scene
imgEarth !bmp =
    let
        bbmp = bmp
        -- bbmp = flipBitmap bmp
        -- bbmp = mirrorBitmap bmp
        ptex = ImageTexture $! bitmapToImageT bbmp
        -- ptex = SolidTexture $ SolidV ( VList [0.2, 0.3, 0.1] )
        lmb = LambMat $! Lamb {lalbedo = ptex}
        sp2 = SphereObj {sphereCenter = VList [0.0, 0.0, 0.0],
                         sphereRadius = 2,
                         sphereMat = lmb}
        hs = HList [HitSphere sp2]
    in SceneVals {
        img_width = imageWidth,
        aspect_ratio = aspectRatio,
        img_height = imageHeight,
        nb_samples = nbSamples,
        bounce_depth = bounceDepth,
        cam_look_from = VList [0.0, 0.0, 12.0],
        cam_look_to = camLookTo,
        cam_vfov = camVFov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        back_ground = VList [0.7, 0.8, 1.0]
    }

-- simple light scene
simpleLight :: RandomGen g => g -> Scene
simpleLight g =
    let ptex = NoiseTexture $! mkPerlinNoise g 4.0
        lmb = LambMat $! Lamb {lalbedo = ptex}
        sp1 = SphereObj {sphereCenter = VList [0.0, -1000.0, 0.0],
                         sphereRadius = 1000,
                         sphereMat = lmb}
        sp2 = SphereObj {sphereCenter = VList [0.0, 2.0, 0.0],
                         sphereRadius = 2,
                         sphereMat = lmb}
        lTex = SolidTexture $! SolidV ( VList [4.5, 4.5, 4.5])
        lmat = LightMat $! DLight {emitTexture = lTex}
        dlight = AaQuad $! mkXyRect 3.0 5.0 1.0 3.0 (-2.0) lmat
        sp3 = SphereObj {sphereCenter = VList [0.0, 8.0, 0.0],
                         sphereRadius = 2,
                         sphereMat = lmat}
        hs = HList [HitSphere sp1, HitSphere sp2, dlight, HitSphere sp3]
    in SceneVals {
        img_width = imageWidth,
        aspect_ratio = aspectRatio,
        img_height = imageHeight,
        nb_samples = 200,
        bounce_depth = 100,
        cam_look_from = VList [26.0, 3.0, 6.0],
        cam_look_to = VList [0.0, 2.0, 0.0],
        cam_vfov = camVFov,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        back_ground = VList [0.0, 0.0, 0.0]
    }



chooseScene :: RandomGen g => g -> [Bitmap Word8] -> Int -> (Int, Scene)
chooseScene g s choice =
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
        _ -> let sc = diffuseSphere in (nb_samples sc, sc)

