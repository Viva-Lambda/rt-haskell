-- pixel
module Color.Pixel where

import Math3D.Vector
import Math3D.CommonOps

import Color.ColorInterface

--
import Spectral.SampledSpectrum
import Spectral.SampledDistribution

-- 
import Debug.Trace

data PixelSpectrum = PixSpecTrichroma (Double, Double, Double)
                   | PixSpecSampled SampledSpectrum
                   deriving (Eq, Show)

zeroPixelSpectrum :: PixelSpectrum
zeroPixelSpectrum = PixSpecTrichroma (0.0, 0.0, 0.0)

pixelCheck :: PixelSpectrum -> PixelSpectrum -> (Bool, String)
pixelCheck a c =
    case a of
       PixSpecTrichroma _ ->
            case c of
                PixSpecTrichroma _ -> (True, "Pixel is of same type")
                PixSpecSampled _ -> (False, "Pixel spectrums are not of same type")
       PixSpecSampled _ ->
            case c of
                PixSpecTrichroma _ -> (False, "Pixel spectrums are not of same type")
                PixSpecSampled _ -> (True, "Pixel is of same type")


instance BinaryOps PixelSpectrum where
    elementwiseOp str f a b =
        let (isSame, s) = pixelCheck a b
        in if isSame == False
           then traceStack (s ++ " :: " ++ str) zeroPixelSpectrum
           else case a of
                    PixSpecTrichroma (r1, g1, b1) ->
                        case b of
                            PixSpecTrichroma (r2, g2, b2) ->
                                let v1 = fromList2Vec r1 [g1, b1]
                                    v2 = fromList2Vec r2 [g2, b2]
                                    res = elementwiseOp str f v1 v2
                                    [rr, gg, bb] = vec2List $! res
                                in PixSpecTrichroma (rr, gg, bb)
                            PixSpecSampled _ ->
                                traceStack (s ++ " :: " ++ str) zeroPixelSpectrum
                    PixSpecSampled s1 ->
                        case b of
                            PixSpecTrichroma _ ->
                                traceStack (s ++ " :: " ++ str) zeroPixelSpectrum
                            PixSpecSampled s2 ->
                                PixSpecSampled $! elementwiseOp str f s1 s2

    elementwiseScalarOp str f a =
        case a of
            PixSpecTrichroma (r, g, b) ->
                let res = elementwiseScalarOp str f (fromList2Vec r [g, b])
                    [rr, gg, bb] = vec2List $! res
                in PixSpecTrichroma (rr, gg, bb)
            PixSpecSampled s -> PixSpecSampled $! elementwiseScalarOp str f s

    -- division
    divide a b =
        let (isSame, str) = pixelCheck a b
        in if isSame == False
           then traceStack (str) zeroPixelSpectrum
           else case a of
                    PixSpecTrichroma (r1, g1, b1) ->
                        case b of
                            PixSpecTrichroma (r2, g2, b2) ->
                                let v1 = fromList2Vec r1 [g1, b1]
                                    v2 = fromList2Vec r2 [g2, b2]
                                    res = divide v1 v2
                                    [rr, gg, bb] = vec2List $! res
                                in PixSpecTrichroma (rr, gg, bb)
                            PixSpecSampled _ ->
                                traceStack str zeroPixelSpectrum
                    PixSpecSampled s1 ->
                        case b of
                            PixSpecTrichroma _ ->
                                traceStack str zeroPixelSpectrum
                            PixSpecSampled s2 -> PixSpecSampled $! divide s1 s2


toColorRecord :: PixelSpectrum -> Word -> ColorRecord
toColorRecord a wave = case a of
                         PixSpecTrichroma (r,g,b) -> ColorRec {
                            model = ColorRGB $! fromList2Vec r [g, b]
                            }
                         PixSpecSampled s -> 
                            let v = evaluateWave wave (sampled s)
                            in ColorRec {
                                model = ColorSpec (spectrumType s, (wave, v))
                            }

data Pixel = Pix {x :: Int, y :: Int,
                  color :: PixelSpectrum} deriving (Eq, Show)
