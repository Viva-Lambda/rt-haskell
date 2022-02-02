{-# LANGUAGE BangPatterns #-}
-- image texture
module Texture.Image where

-- math related
import Math3D.Vector
import Math3D.CommonOps
-- color related
import Color.ColorInterface
--
import Texture.Texture
--
import Utility.Utils

-- thirdparty
import GHC.Float hiding (clamp)
import Data.Word
import Data.Bitmap.Pure.Pixels
import Data.Bitmap.Base
import Codec.Image.STB
import qualified  Data.Map as DMap
import Data.List
import Debug.Trace

type Row = Int
type Column = Int

type ImageData = [[Word8]]

data ImageT = ImgT {
        imgTWidth :: Int,
        imgTHeight :: Int,
        bytesPerScanline :: Int,
        bytesPerPixel :: Int,
        imgData :: ImageData
    }

instance Show ImageT where
    show (ImgT {imgTWidth = a, imgTHeight = b, 
                bytesPerScanline = c,
                bytesPerPixel = d, imgData = _}) = 
        let msg1 = "<Image Texture: " ++ (show a) ++ "x" ++ (show b)
            msg2 = "x" ++ (show d) ++ ">"
        in msg1 ++ msg2


debugImageT :: ImageT -> String
debugImageT img = 
    let imdata = imgData img
        msg1 = show img
        msg2 = show imdata
    in msg1 ++ " data: " ++ msg2

--
pixToDouble :: Word8 -> Double
pixToDouble p = fromRational $ toRational p

bitmapToImageT :: Image -> ImageT
bitmapToImageT !b =
    let (w, h) = bitmapSize b
        channels = bitmapNChannels b
        offsets = [(ww, hh) | hh <- [0..(h-1)], ww <- [0..(w-1)] ]
        readerFn oset = let p = unsafeReadPixel b oset
                        in p
        -- foldfn :: (a -> b -> a) :: ([] -> [a] -> [a])
        imdata = map (readerFn) offsets
        -- imindx = [0..((length imdata) - 1)]
        imgt = ImgT {imgTWidth = w, imgTHeight = h,
                     bytesPerPixel = channels,
                     bytesPerScanline = channels * w,
                     imgData = imdata}
    in imgt
    -- in error $ debugImageT imgt


instance Texture ImageT where
    color !imgt !u !v p _ =
        let (ImgT {imgTWidth = a, imgTHeight = b, bytesPerScanline = bps,
                     bytesPerPixel = bpp, imgData = imap}) = imgt
        in if null imap
           then ColorRec { model = ColorRGB $! fromList2Vec 0.0 [1.0, 1.0] }
           else let uu = clamp u 0.0 1.0
                    vv = 1.0 - (clamp v 0.0 1.0)
                    -- vv = clamp v 0.0 1.0
                    -- uu = 1.0 - (clamp u 0.0 1.0)
                    i_ = double2Int $ uu * (int2Double a)
                    j_ = double2Int $ vv * (int2Double b)
                    --
                    i = if i_ >= a
                        then a - 1 
                        else i_
                    j = if j_ >= b
                        then b - 1
                        else j_
                    cscale = 1.0 / 255.0
                    pix = let cs = [0..(bpp-1)]
                              coeff = (j * bps) + (i * bpp)
                              iis = [coeff + c | c <- cs]
                              -- vals = map pixToDouble [imap !! ii | ii <- iis]
                              pixIndex = let pindex = j * a + i
                                             msg = "negative index row: " ++ show j
                                             msg2 = " image width " ++ show a
                                             msg3 = " image height " ++ show b
                                             msg4 = " column index " ++ show i
                                             msg5 = " uu " ++ show uu
                                             msg6 = " vv " ++ show vv
                                             msg7 = " u " ++ show u
                                             msg8 = " v " ++ show v
                                             msg9 = msg ++ msg2 ++ msg3 ++ msg4
                                             msg10 = msg5 ++ msg6 ++ msg7
                                             msg11 = msg8 ++ msg9 ++ msg10
                                         in if pindex < 0
                                            then traceStack msg11 0
                                            else pindex
                              (val:vals) = map pixToDouble (imap !! pixIndex)
                          -- in VList [imap DMap.! ii | ii <- iis]
                          in fromList2Vec val vals
                    cval = multiplyS pix cscale
                in ColorRec { model = ColorRGB cval }
                -- in error $ show imgt
