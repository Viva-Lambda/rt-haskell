{-# LANGUAGE BangPatterns #-}
-- next week final scene
module Scene.NextWeekFinal(nextWeekFinal) where

-- scene default values
import Scene.Scene

import Color.Pixel
-- 
import System.Random
import Random
import GHC.Float
import Data.Bitmap.Base
import Data.Bitmap.Simple

-- math
import Math3D.Vector
import Math3D.CommonOps
import Math3D.Ray

-- texture
import Texture.SolidColor
import Texture.TextureObj
import Texture.Image
import Texture.Noise

-- hittable
import Hittable.HittableList
import Hittable.HittableObj
import Hittable.Hittable
import Hittable.AaRect
import Hittable.Rotatable
import Hittable.Translatable
import Hittable.MovingSphere
import Hittable.Sphere
import Hittable.Bvh
import Hittable.ConstantMedium

-- instance
import Instance.Box

--
import Camera
import Material.Material

--
import Utility.HelperTypes

mkBoxes :: RandomGen g => g -> (g, [HittableObj])
mkBoxes g =
    let gTexture = TextureCons $! SolidD 0.48 0.83 0.53
        gmat = LambMat $! LambT gTexture
        bcoords = zip (map int2Double [0..20]) (map int2Double [0..20])
        mkbox acc (i, j) = let w = 100.0
                               x0 = -1000.0 + (i * w)
                               z0 = -1000.0 + (j * w)
                               y0 = 0.0
                               x1 = x0 + w
                               (g1, lst) = acc
                               RandResult (y1, g2) = randomDouble g1 (1.0, 101.0)
                               z1 = z0 + w
                               mnp = fromList2Vec x0 [y0, z0]
                               mxp = fromList2Vec x1 [y1, z1]
                           in (g2, lst ++ [HittableCons $ mkBox mnp mxp gmat])
        (g2, boxes) = foldl mkbox (g, []) bcoords
    in (g2, boxes)

mkMovSphere :: HittableObj
mkMovSphere = let c1 = fromList2Vec 400.0 [ 400.0, 200.0]
                  c2 = add c1 (fromList2Vec 30.0 [ 0.0, 0.0])
                  ltex = TextureCons $! SolidD 0.78 0.3 0.1
                  lmat = LambMat $! LambT ltex
              in HittableCons $! MovSphereObj {msphereCenter1 = c1,
                                               msphereCenter2 = c2,
                                               msphereRadius = 50.0,
                                               mTime0 = 0.0,
                                               mTime1 = 1.0,
                                               msphereMat = lmat}

earthImg :: Bitmap Word8 -> HittableObj
earthImg bmp =
    let ptex = TextureCons $! bitmapToImageT bmp
        -- ptex = SolidTexture $ SolidV ( fromList2Vec [0.2, 0.3, 0.1] )
        lmb = LambMat $! LambT ptex 
    in HittableCons $! SphereObj {sphereCenter = fromList2Vec 400.0 [200.0, 400.0],
                                  sphereRadius = 100,
                                  sphereMat = lmb}

noiseSphere :: RandomGen g => g -> (g, HittableObj)
noiseSphere g1 =
    let (g2, noiseT) = mkPerlinNoiseWithSeed g1 0.1
        ptex = TextureCons noiseT
        lmb = LambMat $! LambT ptex 
    in (g2, HittableCons $! SphereObj {
            sphereCenter = fromList2Vec 220.0 [280.0, 300.0],
            sphereRadius = 80.0,
            sphereMat = lmb
        })

mkTransformedBoxes :: RandomGen g => g -> (g, HittableObj)
mkTransformedBoxes g =
    --
    let whiteTexture = TextureCons $! SolidD 0.75 0.75 0.75
        whmat = LambMat $! LambT whiteTexture
        foldlfn acc _ = let (g1, lst) = acc
                            RandResult (rvec, g2) = randomVec (0.0, 165.0) g1
                            sp = HittableCons $! SphereObj {
                                        sphereCenter = rvec,
                                        sphereRadius = 10,
                                        sphereMat = whmat
                                    }
                        in (g2, lst ++ [sp])
        (g1, boxes) = foldl foldlfn (g, []) [0..999]
        bvhboxes = mkBvh boxes g1 0 (length boxes) 0.0 1.0
        rotatedBoxes = mkRotatable bvhboxes 15.0 RY
        transBoxes = Translate bvhboxes (fromList2Vec (-100.0) [270.0, 395.0])
    in (g1, HittableCons transBoxes)


nextWeekFinal :: RandomGen g => g -> Bitmap Word8 -> Scene
nextWeekFinal gen img =
    let (g1, boxes) = mkBoxes gen
        mbvh = mkBvh boxes g1 0 (length boxes) 0.0 1.0
        highWhiteTexture = TextureCons $! SolidD 15.0 15.0 15.0
        lightMat = LightMat $! DLightEmitTextureCons highWhiteTexture
        light = HittableCons $! mkXzRect 123.0 423.0 147.0 412.0 554.0 lightMat
        --
        metTexture = TextureCons $! SolidD 0.8 0.8 0.9
        msphere = mkMovSphere
        dieSp1 = HittableCons $! SphereObj {
                                sphereCenter = fromList2Vec 260.0 [150.0, 45.0],
                                sphereRadius = 50.0,
                                sphereMat = DielMat $! DielRefIndices [1.5]
                                }
        dieSp2 = HittableCons $! SphereObj {
                        sphereCenter = fromList2Vec 0.0 [150.0, 145.0],
                        sphereRadius = 50.0,
                        sphereMat = MetalMat $! MetT metTexture 1.0
                        }
        boundary1 = HittableCons $! SphereObj {
                        sphereCenter = fromList2Vec 360.0 [150.0, 145.0],
                        sphereRadius = 70.0,
                        sphereMat = DielMat $! DielRefIndices [1.5]
                        }
        cmed1Texture = TextureCons $! SolidD 0.2 0.4 0.9
        cmed1 = HittableCons $! mkConstantMedium boundary1 0.2 cmed1Texture

        boundary2 = HittableCons $! SphereObj {
                        sphereCenter = zeroV3,
                        sphereRadius = 5000.0,
                        sphereMat = DielMat $! DielRefIndices [1.5]
                        }

        cmed2Texture = TextureCons $! SolidD 1.0 1.0 1.0
        cmed2 = HittableCons $! mkConstantMedium boundary2 0.00001 cmed2Texture
        eimg = earthImg img
        (g2, noiseS) = noiseSphere g1
        (g3, tboxes) = mkTransformedBoxes g2
        objlst = [HittableCons mbvh,
            light, msphere, dieSp1, dieSp2, boundary1,
            cmed1, cmed2, eimg, noiseS, tboxes
            ]
        hs = HList {objects = NList (
            HittableCons $ mkBvh objlst g3 0 (length objlst) 0.0 1.0
            ) []}
    -- in error $ "\nN: " ++ show b2 ++ "\nR: " ++ show b2rot ++ "\nT: " ++ show b2trans
    in SceneVals {
        img_width = imageWidth,
        -- img_width = 800,
        aspect_ratio = aspectRatio,
        -- aspect_ratio = 800.0 / 600.0,
        img_height = imageHeight,
        -- img_height = 600,
        nb_samples = 100,
        bounce_depth = 20,
        cam_look_from = fromList2Vec 478.0 [ 278.0, -600.0],
        cam_look_to = fromList2Vec 278.0 [ 278.0, 0.0],
        cam_vfov = 40.0,
        cam_vup = camVUp,
        cam_focus_distance = camFocDistance,
        cam_aperture = 0.0,
        scene_obj = hs,
        sample_obj = HList {objects = NList light [dieSp1,boundary1,tboxes]},
        back_ground = PixSpecTrichroma (0.0,0.0,0.0)
    }
