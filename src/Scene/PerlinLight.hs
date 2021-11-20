-- perlin sphere with light module
module Scene.PerlinLight(simpleLight) where

-- default values
import Scene.Scene

-- math3d
import Math3D.Vector
import Math3D.CommonOps

-- texture
import Texture.TextureObj
import Texture.Noise
import Texture.SolidColor

-- hittable
import Hittable.Sphere
import Hittable.HittableObj
import Hittable.HittableList
import Hittable.AaRect

-- material
import Material.Material

--
import System.Random
import Random

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
