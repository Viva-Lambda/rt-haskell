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

import Color.Pixel
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

-- utility
import Utility.HelperTypes


mkRndMat :: RandomGen g => g -> Int -> Int -> Bool -> (Maybe HittableObj, g)
mkRndMat gen !a !b !isMoving =
    let fnlist = fromList2NL randval [randval, randval]
        RandResult (nlst, g3) = randFoldlFixedRange2 gen fnlist
        (chooseMat:cxrand:czrand:_) = nl2List nlst
        center = fromList2Vec ((int2Double a) + (0.9 * cxrand)) [
                                            0.2,
                                            (int2Double b) + (0.9 * czrand)]
        diff = subtract center (fromList2Vec 4.0 [0.2, 0.0])
        cdiff = magnitude diff
    in if cdiff > 0.9
       then if (chooseMat > 0.3) && (chooseMat < 0.8)
            then let fnlst2 = fromList2NL randV [randV]
                     RandResult (nlst2, g5) = randFoldlFixedRange2 g3 fnlst2
                     (rv1:rv2:_) = nl2List nlst2
                     diffAlbedo = multiply rv1 rv2
                     laMat = LambMat $! LambC diffAlbedo
                in if isMoving
                   then let RandResult (rv3, g6) = randomDouble g5 (0.0, 0.5)
                        in (Just $! HittableCons MovSphereObj {
                            msphereCenter1 = center,
                            msphereCenter2 = add center (fromList2Vec 0.0 [rv3, 0.0]),
                            msphereRadius = 0.2,
                            msphereMat = laMat,
                            mTime0 = 0.0,
                            mTime1 = 1.0
                            }, g6)
                   else (Just $! HittableCons SphereObj {sphereCenter = center,
                                                     sphereRadius = 0.2,
                                                     sphereMat = laMat}, g5)
            else if chooseMat >= 0.8 && chooseMat < 0.9
                 then let RandResult (rv1, g4) = randomVec (0.5, 1.0) g3
                          RandResult (fz, g5) = randomDouble g4 (0.0, 0.5)
                          metMat = MetalMat $! MetC rv1 fz
                      in (Just $! HittableCons SphereObj {
                                    sphereCenter = center,
                                    sphereRadius = 0.2,
                                    sphereMat = metMat
                                    }, g5)
                 else let dieMt = DielMat $! DielRefIndices [1.5]
                      in (Just $! HittableCons SphereObj {
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

world :: RandomGen g => g -> Bool -> (HittableList, HittableList)
world gen !isM = let as = [0..7]
                     bs = [0..7]
                     coords = [(a - 3, b - 3) | a <- as, b <- bs]
                     objs = mkRndMats gen isM coords
                     groundMat = LambMat $! LambC (singularV 3 0.5 )
                     ground = HittableCons SphereObj {
                                    sphereCenter = fromList2Vec 0.0 [-1000.0, 0.0],
                                    sphereRadius = 1000.0,
                                    sphereMat = groundMat}
                     dielM1 = DielMat $! DielRefIndices [1.5]
                     lambM2 = LambMat $! LambC (fromList2Vec 0.4 [ 0.2, 0.1])
                     metalM3 = MetalMat $! MetC ( fromList2Vec 0.7 [ 0.6, 0.5] ) 0.0
                     dielObj = HittableCons $! SphereObj {
                            sphereCenter =fromList2Vec 0.0 [ 1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = dielM1 
                        }
                     lambObj = HittableCons $! SphereObj {
                            sphereCenter = fromList2Vec (-4.0) [1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = lambM2
                        }
                     metObj = HittableCons $! SphereObj {
                            sphereCenter =  fromList2Vec 4.0 [ 1.0, 0.0],
                            sphereRadius = 1.0,
                            sphereMat = metalM3
                        }
                  in if null objs
                     then error $ unwords (map show objs)
                     else (HList {
                         objects = NList ground (objs ++ [dielObj, lambObj, metObj])
                     }, HList {objects = NList (head objs) (tail objs)})


worldStat :: RandomGen g => g -> (HittableList, HittableList)
worldStat g = world g False

worldMoving :: RandomGen g => g -> (HittableList, HittableList)
worldMoving g = world g True

-- book scenes


randomOneWeekendFinalScene :: RandomGen g => g -> Bool -> Scene
randomOneWeekendFinalScene g b =
    let (hl, sobjs) = world g b
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
        sample_obj = sobjs,
        back_ground = PixSpecTrichroma (0.7,0.8,1.0)
    }

randomOneWeekendFinalSceneStatic ::RandomGen g => g -> Scene
randomOneWeekendFinalSceneStatic g = randomOneWeekendFinalScene g False

randomOneWeekendFinalSceneMove ::RandomGen g => g -> Scene
randomOneWeekendFinalSceneMove g = randomOneWeekendFinalScene g True
