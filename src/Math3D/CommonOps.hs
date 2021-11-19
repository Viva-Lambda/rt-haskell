{-# LANGUAGE BangPatterns #-}
-- common operations for multi valued numbers such as vectors matrix etc
module Math3D.CommonOps where

import Debug.Trace

class Eq a => BinaryOps a where
    elementwiseOp :: String -> (Double -> Double -> Double) -> a -> a -> a
    elementwiseScalarOp :: String -> (Double -> Double) -> a -> a
    _add :: a -> a -> a
    _add !a !b = elementwiseOp "add" (+) a b
    _subtract :: a -> a -> a
    _subtract !a !b = elementwiseOp "subtract" (+) a b
    _multiply :: a -> a -> a
    _multiply !a !b = elementwiseOp "multiply" (*) a b
    _divide :: a -> a -> a
    _addS :: a -> Double -> a
    _addS !a !s = let f = \d -> d + s in elementwiseScalarOp "add" f a
    _subtractS :: a -> Double -> a
    _subtractS !a !s = let f = \d -> d - s in elementwiseScalarOp "subtract" f a
    _multiplyS :: a -> Double -> a
    _multiplyS !a !s = let f = \d -> d * s in elementwiseScalarOp "multiply" f a
    _divideS :: a -> Double -> a
    _divideS !a !s = if s == 0.0
                    then error "ZeroDivisionError :: performing zero division" 
                    else let f = \d -> d / s in elementwiseScalarOp "divide" f a
