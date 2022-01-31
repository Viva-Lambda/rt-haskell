{-# LANGUAGE BangPatterns #-}
-- input output of colors
module Color.ColorInterface where

import Math3D.Vector
import Math3D.CommonOps

import Spectral.SampledSpectrum

-- thirdparty
import Debug.Trace

data ColorFlag = RGB
                | Spectral SpectrumType
                deriving (Show, Eq)

data ColorModel = ColorRGB Vector
                | ColorSpec (SpectrumType, (Word, Double))
                deriving (Show, Eq)

data ColorRecord = ColorRec { model :: ColorModel } deriving (Eq, Show)

emptyRGBRecord :: ColorRecord
emptyRGBRecord = ColorRec { model = ColorRGB zeroV3 }

stype :: ColorRecord -> ColorFlag
stype a = case model a of
                ColorRGB _ -> RGB
                ColorSpec (b, _) -> Spectral b

-- obtain color data: we upcast everything to vector
colorData :: ColorRecord -> Vector
colorData a = case model a of
                ColorRGB  v -> v
                ColorSpec (_, (_,v)) -> fromList2Vec v []

emptyModelLike :: ColorRecord -> ColorRecord
emptyModelLike a = case model a of
                        ColorRGB v -> ColorRec {model = ColorRGB $! zeroLikeVector v}
                        ColorSpec (s, _) -> ColorRec {model = ColorSpec (s, (0, -1.0)) }

colorModelCheck :: ColorRecord -> ColorRecord -> (Bool, String)
colorModelCheck a b =
    let msg1 = "Color Models of interfaces are not the same: "
        msg2 = show (stype a)
        msg3 = show (stype b)
    in ((stype a) == (stype b), msg1 ++ msg2 ++ " " ++ msg3 )

instance BinaryOps ColorRecord where
    elementwiseOp str f a b =
        let (isSame, s) = colorModelCheck a b
        in if isSame == False
           then traceStack (s ++ " :: " ++ str) emptyRGBRecord
           else case model a of
                    ColorRGB av ->
                        case model b of
                            ColorRGB bv -> 
                                ColorRec {model = (ColorRGB $! vecArithmeticOp str f av bv)}
                            _ -> traceStack (s ++ " :: " ++ str) emptyRGBRecord
                    ColorSpec (sa, (aw, apower)) ->
                        case model b of
                            ColorSpec (sb, (bw, bpower)) ->
                                if aw == bw
                                then ColorRec { model = ColorSpec (sa, (aw, f apower bpower)) }
                                else let msg1 = "wavelengths are not same" 
                                         msg2 = " for given spectral powers"
                                         msg3 = ", this library is not"
                                         msg4 = " equiped to cover such cases"
                                         msg5 = " for sampled spectrums."
                                         msg = msg1 ++ msg2 ++ msg3 ++ msg4
                                     in traceStack (msg ++ msg5) emptyRGBRecord
                            _ -> traceStack (s ++ " :: " ++ str) emptyRGBRecord

    elementwiseScalarOp str f a = 
        case model a of
            ColorRGB av -> ColorRec {model = ColorRGB $! vecScalarOp f av}
            ColorSpec (s, (aw, p)) -> ColorRec {model = ColorSpec (s, (aw, f p))}

    -- division
    divide a b =
        let (isSame, str) = colorModelCheck a b
        in if isSame == False
           then traceStack (str) emptyRGBRecord
           else case model a of
                    ColorRGB av ->
                        case model b of
                            ColorRGB bv -> 
                                ColorRec {model = ColorRGB $! divide av bv }
                            _ -> traceStack str emptyRGBRecord
                    ColorSpec (sa, (aw, apower)) ->
                        case model b of
                            ColorSpec (sb, (bw, bpower)) ->
                                if aw == bw
                                then if bpower /= 0.0 
                                     then ColorRec {model = ColorSpec (sa, (aw, apower / bpower)) }
                                     else traceStack 
                                            "zero division in color models"
                                            emptyRGBRecord
                                else let msg1 = "wavelengths are not same" 
                                         msg2 = " for given spectral powers"
                                         msg3 = ", this library is not"
                                         msg4 = " equiped to cover such cases"
                                         msg5 = " for sampled spectrums."
                                         msg = msg1 ++ msg2 ++ msg3 ++ msg4
                                     in traceStack (msg ++ msg5) emptyRGBRecord
                            _ -> traceStack str emptyRGBRecord


fromRGB :: Double -> Double -> Double -> ColorRecord
fromRGB a b c = ColorRec { model = ColorRGB $! fromList2Vec a [b, c] }

