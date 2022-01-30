{-# LANGUAGE BangPatterns #-}
-- input output of colors
module Color.ColorInterface where

import Math3D.Vector
import Math3D.CommonOps

import Spectral.SampledSpectrum

-- thirdparty
import Debug.Trace

data ColorModel = RGB
                | Spectral SpectrumType
                deriving (Show, Eq)

data ColorInterface = ColorInt {
    stype :: ColorModel,
    colorData :: Vector
    } deriving (Eq, Show)

emptyRGBModel :: ColorInterface
emptyRGBModel = ColorInt {stype = RGB, colorData = zeroV3}

emptyModelLike :: ColorInterface -> ColorInterface
emptyModelLike a = 
    let atype = stype a
    in if atype == RGB
       then ColorInt {stype = atype, colorData = zeroV3}
       else ColorInt {stype = atype, colorData = zeroLikeVector $! colorData a}

colorModelCheck :: ColorInterface -> ColorInterface -> (Bool, String)
colorModelCheck a b =
    ((stype a) == (stype b), "Color Models of interfaces are not the same")

instance BinaryOps ColorInterface where
    elementwiseOp str f a b =
        let (isSame, s) = colorModelCheck a b
        in if isSame == False
           then traceStack (s ++ " :: " ++ str) emptyRGBModel
           else let ap = colorData a
                    bp = colorData b
                    ndata = vecArithmeticOp str f ap bp
                in ColorInt {stype = stype a, colorData = ndata}

    elementwiseScalarOp str f a = let ap = colorData a
                                      ndata = vecScalarOp f ap
                                  in ColorInt { stype = stype a,
                                                colorData = ndata }
    -- division
    divide a b =
        let (isSame, str) = colorModelCheck a b
        in if isSame == False
           then traceStack (str) emptyRGBModel
           else let ap = colorData a
                    bp = colorData b
                    ndata = divide ap bp
                in ColorInt { stype = stype a, colorData = ndata}

fromPowers :: Vector -> ColorInterface
fromPowers a = ColorInt {
        stype = Spectral ILLUMINANT,
        colorData = a
    }

fromRGB :: Double -> Double -> Double -> ColorInterface
fromRGB a b c = ColorInt {
        stype = RGB,
        colorData = fromList2Vec a [b, c]
    }
