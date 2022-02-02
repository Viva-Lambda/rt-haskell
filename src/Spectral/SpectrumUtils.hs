-- spectral utils
module Spectral.SpectrumUtils where

-- math related
import Math3D.Matrix
import Math3D.Vector
import Math3D.CommonOps

-- utilities
import Utility.HelperTypes

xyzRgb_transform :: Vector -> (Vector, Vector, Vector, Vector) -> Vector
xyzRgb_transform xyz (row1, row2, row3, row4) =
    let m = mzero 4 4
        m1 = msetRow m 0 row1
        m2 = msetRow m1 1 row2
        m3 = msetRow m2 2 row3
        m4 = msetRow m3 3 row4
        (x, y, z) = (vget xyz 0, vget xyz 1, vget xyz 2)
        v = fromList2Vec x [y, z, 0.0]
        mvec = matFromVector (fromList2NL v [])
        mat = matmul mvec m4
        [r,g,b,w] = nl2List $! mdata mat
    in fromList2Vec r [g, b]


xyz2rgb_cie :: Vector -> Vector
xyz2rgb_cie xyz = let row1 = fromList2Vec 2.3706743 [-0.5138850, 0.0052982, 0.0]
                      row2 = fromList2Vec (-0.9000405) [1.4253036, -0.0146949, 0.0]
                      row3 = fromList2Vec (-0.4706338) [0.0885814, 1.0093968, 0.0]
                      row4 = fromList2Vec 0.0 [0.0, 0.0, 1.0]
                  in xyzRgb_transform xyz (row1, row2, row3, row4)

xyz2rgb_srgb :: Vector -> Vector
xyz2rgb_srgb xyz = let row1 = fromList2Vec 3.2404542 [-0.9692660, 0.0556434, 0.0]
                       row2 = fromList2Vec (-1.5371385) [1.8760108, -0.2040259, 0.0]
                       row3 = fromList2Vec (-0.4985314) [0.0415560, 1.0572252, 0.0]
                       row4 = fromList2Vec 0.0 [0.0, 0.0, 1.0]
                   in xyzRgb_transform xyz (row1, row2, row3, row4)

xyz2rgb_pbr :: Vector -> Vector
xyz2rgb_pbr xyz = let (x, y, z) = (vget xyz 0, vget xyz 1, vget xyz 2)
                      r = x * 3.240479 - 1.537150 * y - 0.498535 * z
                      g = x * (-0.969256) + 1.875991 * y + 0.041556 * z
                      b = x * 0.055648 - 0.204043 * y + 1.057311 * z
                  in fromList2Vec r [g, b]


rgb2xyz_srgb :: Vector -> Vector
rgb2xyz_srgb rgb = let row1 = fromList2Vec 0.4124564 [0.2126729, 0.0193339, 0.0]
                       row2 = fromList2Vec 0.3575761 [0.7151522, 0.1191920, 0.0]
                       row3 = fromList2Vec 0.1804375 [0.0721750, 0.9503041, 0.0]
                       row4 = fromList2Vec 0.0 [0.0, 0.0, 1.0]
                   in xyzRgb_transform rgb (row1, row2, row3, row4)

rgb2xyz_cie :: Vector -> Vector
rgb2xyz_cie rgb = let row1 = fromList2Vec 0.4887180 [0.1762044, 0.0, 0.0]
                      row2 = fromList2Vec 0.3106803 [0.8129847, 0.0102048, 0.0]
                      row3 = fromList2Vec 0.2006017 [0.0108109, 0.9897952, 0.0]
                      row4 = fromList2Vec 0.0 [0.0, 0.0, 1.0]
                  in xyzRgb_transform rgb (row1, row2, row3, row4)
