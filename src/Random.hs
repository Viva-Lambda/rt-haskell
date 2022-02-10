{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
-- random related
module Random where

import System.Random
import GHC.Float
import Data.Functor

-- utility functions
import Utility.HelperTypes
import Utility.BaseEnum


data RandomResult a g where
    RandResult :: RandomGen g => (a, g) -> RandomResult a g


randomFn :: (Random a, Ord a, RandomGen g) => g -> (a, a) -> RandomResult a g
randomFn gen (!low, !high) = if low > high
                             then ranD high low
                             else ranD low high
    where ranD lval hval =
            let (a, g) = randomR (lval, hval) gen in RandResult (a, g)


rfmap :: RandomGen g => (a -> b) -> RandomResult a g -> RandomResult b g
rfmap f a = case a of
                (RandResult (b, g)) -> RandResult (f b, g)

{-
given a random result and a function that makes random result from two-tuple
and a two-tuple make a random result
-}
randomChain :: (Ord a, RandomGen g) => RandomResult a g -> (g -> (a, a) -> RandomResult a g) -> (a, a) -> RandomResult a g

randomChain res rf mnmx = case res of
                             RandResult (_, g) -> rf g mnmx

liftRandVal :: RandomGen g => RandomResult a g -> a
liftRandVal a = case a of
                    RandResult (b, _) -> b
liftRandGen :: RandomGen g => RandomResult a g -> g
liftRandGen a = case a of
                    RandResult (_, b) -> b

randFoldl :: RandomGen g => g -> NonEmptyList (g -> (a, a) -> RandomResult a g, (a, a)) -> RandomResult (NonEmptyList a) g
randFoldl gen foldableFns =
    let foldfn acc fn = let (lst, g) = acc
                            (f, (mn, mx)) = fn
                            RandResult (b, g2) = f g (mn, mx)
                        in (lst ++ [b], g2)
        (m:ms, g) = foldl foldfn ([], gen) (nl2List foldableFns)
    in RandResult (fromList2NL m ms, g)

randFoldlFixedRange :: RandomGen g => g -> (a, a) -> NonEmptyList (g -> (a, a) -> RandomResult a g) -> RandomResult (NonEmptyList a) g

randFoldlFixedRange gen r foldableFns =
    let foldfn acc fn = let (lst, g) = acc
                            RandResult (b, g2) = fn g r
                        in (lst ++ [b], g2)
        (m:ms, g) = foldl foldfn ([], gen) (nl2List foldableFns)
    in RandResult (fromList2NL m ms, g)


randFoldlFixedRange2 :: RandomGen g => g -> NonEmptyList (g -> RandomResult a g) -> RandomResult (NonEmptyList a) g

randFoldlFixedRange2 gen foldableFns =
    let foldfn acc fn = let (lst, g) = acc
                            RandResult (b, g2) = fn g
                        in (lst ++ [b], g2)
        (m:ms, g) = foldl foldfn ([], gen) (nl2List foldableFns)
    in RandResult (fromList2NL m ms, g)


randMap :: (Ord a, RandomGen g) => g -> (g -> (a, a) -> RandomResult a g) -> NonEmptyList (a,a) -> RandomResult (NonEmptyList a) g

randMap gen f ranges =
    let rs = nl2List ranges
        fn acc r = let (alst, g) = acc
                       rval = f g r
                   in case rval of
                        (RandResult (b, g2)) -> (alst ++ [b], g2)
        (vals, g2) = foldl fn ([], gen) rs
    in RandResult (NList (head vals) (tail vals), g2)


randomDouble :: RandomGen g => g -> (Double, Double) -> RandomResult Double g
randomDouble = randomFn 

randomInt :: RandomGen g => g -> (Int, Int) -> RandomResult Int g
randomInt = randomFn

randomWord :: RandomGen g => g -> (Word, Word) -> RandomResult Word g
randomWord = randomFn

randomWaveVal :: RandomGen g => g -> (WaveVal, WaveVal) -> RandomResult WaveVal g
randomWaveVal = randomFn

randomPowerVal :: RandomGen g => g -> (PowerVal, PowerVal) -> RandomResult PowerVal g
randomPowerVal = randomFn

randval :: RandomGen g => g -> RandomResult Double g
randval g = randomDouble g (0.0, 1.0)
