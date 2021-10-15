{-# LANGUAGE BangPatterns #-}
-- perlin noise texture module
module Texture.Noise where

import Vector
import Random
import System.Random
import Texture.Texture
import GHC.Float
-- import Prelude hiding(foldl')
import Data.Foldable

import qualified Data.Map as DMap hiding (foldl')

import Data.Bits

type NMap = DMap.Map Int Int

data Perlin = PNoise {
        prandVec :: [Vector],
        perm_x :: [Int],
        perm_y :: [Int],
        perm_z :: [Int]
    } -- PNoise [Vector] [Int] [Int] [Int]

swapEl :: Int -> Int -> [Int] -> [Int]
swapEl !i !j !iis = let eli = iis !! i 
                        elj = iis !! j
                        lft = take i iis
                        mid = take (j - i - 1) (drop (i + 1) iis)
                        right = drop (j+1) iis
                    in lft ++ [elj] ++ mid ++ [eli] ++ right


perlinInnerPermute :: RandomGen g => g -> Int -> [Int] -> [Int]
perlinInnerPermute !g !i !xs =
    let (target, _) = randomInt g 0 i
    in swapEl target i xs

perlinPermute :: RandomGen g => [g] -> [Int] -> [Int]
perlinPermute gens points =
    let ns = reverse points
        rvals = [(gv, i) | gv <- gens, i <- ns]
        -- foldFn :: (a -> b -> a) :: (nmap -> (i1, i2) -> nmap)
        foldFn acc indx = let (g, idx) = indx in perlinInnerPermute g idx acc
    in foldl' foldFn ns rvals


type Triplet a = (a, a, a)
type Pair a = (a, a)
type MultiPair a = (((a, a), (a, a)),((a, a), (a, a)))

emptyMPairV :: Int -> MultiPair Vector
emptyMPairV size = 
    let a = zeroV size in (((a, a), (a, a)),((a, a), (a, a)))

emptyMPairV3 :: MultiPair Vector
emptyMPairV3 = emptyMPairV 3
mkMPair :: a -> a -> a -> a -> a -> a -> a -> a -> MultiPair a
mkMPair a1 a2 a3 a4 a5 a6 a7 a8 = (((a1, a2), (a3, a4)),((a5, a6), (a7, a8)))

getMPairV :: Int -> Int -> Int -> MultiPair Vector -> Vector
getMPairV i j k a =
    if i == 1
    then let i1 = snd a
         in if j == 1
            then let j1 = snd i1
                 in if k == 1
                    then snd j1
                    else fst j1
            else let j0 = fst i1
                 in if k == 1
                    then snd j0
                    else fst j0
    else let i1 = fst a
         in if j == 1
            then let j1 = snd i1
                 in if k == 1
                    then snd j1
                    else fst j1
            else let j0 = fst i1
                 in if k == 1
                    then snd j0
                    else fst j0

getMPairVTrip :: Triplet Int -> MultiPair Vector -> Vector
getMPairVTrip (i, j, k) = getMPairV i j k

replaceMPairV :: Int -> Int -> Int -> Vector -> MultiPair Vector -> MultiPair Vector
replaceMPairV !i !j !k !v !mpv =
    let (((b, c), (d, e)),((f, g), (h, l))) = mpv
    in if i == 1
       then if j == 1
            then if k == 1 -- i = 1, j = 1, k = 1
                 then (((b, c), (d, e)),((f, g), (h, v)))
                 else (((b, c), (d, e)),((f, g), (v, l)))
            else if k == 1 -- i = 1, j = 0, k = 1
                 then (((b, c), (d, e)),((f, v), (h, l)))
                 else (((b, c), (d, e)),((v, g), (h, l)))
       else if j == 1
            then if k == 1 -- i = 0, j = 1, k = 1
                 then (((b, c), (d, v)),((f, g), (h, l)))
                 else (((b, c), (v, e)),((f, g), (h, l)))
            else if k == 1 -- i = 0, j = 0, k = 1
                 then (((b, v), (d, e)),((f, g), (h, l)))
                 else (((v, c), (d, e)),((f, g), (h, l)))


perlinInnerInterp :: Triplet Double -> Triplet Double -> Triplet Int -> Vector -> Double
perlinInnerInterp !(uu, vv, ww) !(u, v, w) !(i, j, k) !cijk =
    let ijk_uvw a b = a * b + (1 - a) * (1 - b)
        ifl = int2Double i
        jfl = int2Double j
        kfl = int2Double k
        weight = VList [u - ifl, v - jfl, w - kfl]
        wdot = dot weight cijk
        jvv = ijk_uvw jfl vv
        iuu = ijk_uvw ifl uu
        kww = ijk_uvw kfl ww
    in jvv * iuu * kww * wdot

perlinInterp :: MultiPair Vector -> Triplet Double -> Double
perlinInterp c uvw =
    let intTrips = [(i,j,k) | i <- [0..2], j <- [0..2], k <- [0..2]]
        fuvw a = a * a * (3 - 2 * a)
        (u, v, w) = uvw
        uu = fuvw u
        vv = fuvw v
        ww = fuvw w
        acc = 0.0
        -- foldfn (a -> b -> a) :: (Double -> Triplet Int -> Double)
        foldfn a trips = let v = getMPairVTrip trips c
                         in a + (perlinInnerInterp (uu, vv, ww) uvw trips v)
    in foldl' foldfn acc intTrips

perlinInnerNoise :: Perlin -> Triplet Int -> Triplet Int -> MultiPair Vector -> MultiPair Vector

perlinInnerNoise !prln !(i,j,k) !(di, dj, dk) !mpv =
    let PNoise {prandVec = vs, perm_x = xs, perm_y = ys,
                perm_z = zs} = prln
        xval = xs !! ( (i + di) .&. 255)
        yval = ys !! ( (j + dj) .&. 255 )
        zval = zs !! ( (k + dk) .&. 255)
        rvec = vs !! ( xor (xor xval yval) zval )
    in replaceMPairV i j k rvec mpv


perlinNoise :: Perlin -> Vector -> Double
perlinNoise !prln !(VList [px, py, pz]) =
    let (PNoise {prandVec = vs,
                 perm_x = xs,
                 perm_y = ys,
                 perm_z = zs}) = prln
        vlen = vsize (head vs)
        i = double2Int px
        j = double2Int py
        k = double2Int pz
        u = px - (int2Double i)
        v = py - (int2Double j)
        w = pz - (int2Double k)
        intTrips = [(di,dj,dk) | di <- [0..2], dj <- [0..2], dk <- [0..2]]
        nbDims = vsize (head vs)
        cvec = emptyMPairV vlen
        -- foldfn (a -> b -> a) :: (MultiPair Vector -> Triplet Int -> MultiPair Vector)
        foldfn acc trip =  perlinInnerNoise prln (i,j,k) trip acc
        noiseV = foldl' foldfn cvec intTrips
    in perlinInterp noiseV (u,v,w)

perlinTurbulance :: Perlin -> Vector -> Int -> Double
perlinTurbulance !prln !point !depth =
    let acc = 0.0
        tpoint = point
        weight = 1.0
        -- foldfn (a -> b -> a) :: ((Double, Vector, Double) -> Int -> (Double, Vector, Double))
        foldfn accs d = let (a, tmp, w) = accs
                            noiseVal = perlinNoise prln tmp 
                            naval = noiseVal * w + a
                            nweight = w * 0.5
                            ntmp = multiplyS tmp 2
                        in (naval, ntmp, nweight)
        (nAcc, ntemp, nweight) = foldl' foldfn (acc, tpoint, weight) [0..depth]
    in abs nAcc

mkPerlin :: RandomGen g => g -> Int -> Int -> Perlin
mkPerlin g nb_points vecSize =
    let gens = randomGens g nb_points
        (vs, gs) = unzip [(randomVecGen (-1.0, 1.0) gen vecSize) | gen <- gens]
        unitvs = map toUnit vs
        points = [0..(nb_points - 1)]
        x_ps = perlinPermute gs points
        y_ps = perlinPermute gs points
        z_ps = perlinPermute gs points
    in PNoise {perm_x = x_ps, perm_y = y_ps, perm_z = z_ps, prandVec = unitvs}

mkPerlinV3 :: RandomGen g => g -> Int -> Perlin
mkPerlinV3 g nb_points = mkPerlin g nb_points 3

perlinDefault :: RandomGen g => g -> Perlin
perlinDefault g = mkPerlinV3 g 256

data PerlinNoise = PerN { noiseScale :: Double, noiseGen :: Perlin }

mkPerlinNoise :: RandomGen g => g -> Double -> PerlinNoise
mkPerlinNoise !g !scale = PerN { noiseScale = scale, noiseGen = perlinDefault g}

instance Texture PerlinNoise where
    color !(PerN {noiseScale = s, noiseGen = p}) !hu !hv !hp =
        let pz = vget hp 2
            pzscale = s * pz
            depth = 7
            pnoise = perlinTurbulance p hp depth
            pnoiseS = 10 * pnoise
            nval = 1 + sin ( pzscale + pnoiseS )
            nvalHalf = 0.5 * nval
        in VList [nvalHalf, nvalHalf, nvalHalf]
