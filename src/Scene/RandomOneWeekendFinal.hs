{-# LANGUAGE BangPatterns #-}
-- one weekend final scene
module Scene.RandomOneWeekendFinal(
        randomOneWeekendFinalScene,
        randomOneWeekendFinalSceneMove,
        randomOneWeekendFinalSceneStatic
        ) where

-- 
import System.Random
import GHC.Float
import Prelude hiding(subtract)

-- scene defaults
import Scene.Scene

--
import Hittable.HittableList
import Hittable.HittableObj
import Hittable.Sphere
import Hittable.MovingSphere

-- texture
import Texture.SolidColor
import Texture.TextureObj

-- material
import Material.Material

-- random
import Random

-- math3D
import Math3D.Vector
import Math3D.CommonOps


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
