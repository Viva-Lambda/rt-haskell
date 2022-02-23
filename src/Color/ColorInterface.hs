{-# LANGUAGE BangPatterns #-}
-- input output of colors
module Color.ColorInterface where

import Math3D.Vector
import Math3D.CommonOps

import Spectral.SampledSpectrum

import Utility.BaseEnum

-- thirdparty
import Debug.Trace

data ColorFlag = RGB
                | Spectral SpectrumType
                deriving (Show, Eq)

data ColorModel = ColorRGB Vector
                | ColorSpec (SpectrumType, (WaveVal, PowerVal))
                deriving (Show, Eq)

data ColorRecord = ColorRec { model :: ColorModel } deriving (Eq, Show)

emptyRGBRecord :: ColorRecord
emptyRGBRecord = ColorRec { model = ColorRGB zeroV3 }

stype :: ColorRecord -> ColorFlag
stype !a = case model a of
                ColorRGB _ -> RGB
                ColorSpec (b, _) -> Spectral b

-- obtain color data: we upcast everything to vector
colorData :: ColorRecord -> Vector
colorData !a = case model a of
                ColorRGB  v -> v
                ColorSpec (_, (_,v)) -> fromList2Vec v []

emptyModelLike :: ColorRecord -> ColorRecord
emptyModelLike !a = case model a of
                        ColorRGB v -> ColorRec {model = ColorRGB $! zeroLikeVector v}
                        ColorSpec (s, (w, _)) -> ColorRec {model = ColorSpec (s, (w, 0.0)) }

colorModelCheck :: ColorRecord -> ColorRecord -> (Bool, String)
colorModelCheck !a !b =
    let sa = stype a
        sb = stype b
        isEqual = case sa of
                    RGB -> case sb of
                                RGB -> True
                                _ -> False
                    Spectral _ -> case sb of
                                    Spectral _ -> True
                                    _ -> False
        msg1 = "Color Models of interfaces are not the same: "
        msg2 = show (stype a)
        msg3 = show (stype b)
    in (isEqual, msg1 ++ msg2 ++ " " ++ msg3 )

wavelengthStr :: WaveVal -> WaveVal -> String
wavelengthStr !a !b =
    let msg1 = "wavelengths are not same" 
        msg2 = " for given spectral powers"
        msg3 = ", this library is not"
        msg4 = " equiped to cover such cases"
        msg5 = " for sampled spectrums."
        msg6 = msg1 ++ msg2 ++ msg3 ++ msg4 ++ msg5
        msg7 = msg6 ++ " First wavelength " ++ show a
        msg8 = msg7 ++ ", second wavelength " ++ show b
    in msg8


instance BinaryOps ColorRecord where
    {-# INLINE elementwiseOp #-}
    elementwiseOp str f a b =
        let (isSame, s) = colorModelCheck a b
        in if not isSame
           then traceStack (s ++ " :: " ++ str) (emptyModelLike a)
           else case model a of
                    ColorRGB av ->
                        case model b of
                            ColorRGB bv ->
                                ColorRec {model = ColorRGB $! vecArithmeticOp str f av bv}
                            _ -> traceStack (s ++ " :: " ++ str) (emptyModelLike a)
                    ColorSpec (sa, (aw, apower)) ->
                        case model b of
                            ColorSpec (sb, (bw, bpower)) ->
                                if aw == bw
                                then ColorRec { model = ColorSpec (sa, (aw, f apower bpower)) }
                                else let msg = wavelengthStr aw bw
                                     in traceStack (msg ++ " " ++ str) (emptyModelLike a)
                            _ -> traceStack (s ++ " :: " ++ str) (emptyModelLike a)

    elementwiseScalarOp str f a = 
        case model a of
            ColorRGB av -> ColorRec {model = ColorRGB $! vecScalarOp f av}
            ColorSpec (s, (aw, p)) -> ColorRec {model = ColorSpec (s, (aw, f p))}

    -- division
    divide a b =
        let (isSame, str) = colorModelCheck a b
        in if not isSame
           then traceStack str (emptyModelLike a)
           else case model a of
                    ColorRGB av ->
                        case model b of
                            ColorRGB bv -> 
                                ColorRec {model = ColorRGB $! divide av bv }
                            _ -> traceStack str (emptyModelLike a)
                    ColorSpec (sa, (aw, apower)) ->
                        case model b of
                            ColorSpec (sb, (bw, bpower)) ->
                                if aw == bw
                                then if bpower /= 0.0 
                                     then ColorRec {model = ColorSpec (sa, (aw, apower / bpower)) }
                                     else traceStack 
                                            "zero division in color models"
                                            (emptyModelLike a)
                                else let msg = wavelengthStr aw bw
                                     in traceStack msg (emptyModelLike a)
                            _ -> traceStack str (emptyModelLike a)


fromRGB :: Double -> Double -> Double -> ColorRecord
fromRGB !a !b !c = ColorRec { model = ColorRGB $! fromList2Vec a [b, c] }

