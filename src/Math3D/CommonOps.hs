{-# LANGUAGE BangPatterns #-}
-- common operations for multi valued numbers such as vectors matrix etc
module Math3D.CommonOps where

import Debug.Trace

class Eq a => BinaryOps a where
    elementwiseOp :: String -> (Double -> Double -> Double) -> a -> a -> a
    elementwiseScalarOp :: String -> (Double -> Double) -> a -> a
    add :: a -> a -> a
    add !a !b = elementwiseOp "add" (+) a b
    subtract :: a -> a -> a
    subtract !a !b = elementwiseOp "subtract" (-) a b
    multiply :: a -> a -> a
    multiply !a !b = elementwiseOp "multiply" (*) a b
    divide :: a -> a -> a
    addS :: a -> Double -> a
    addS !a !s = let f d = d + s in elementwiseScalarOp "add" f a
    subtractS :: a -> Double -> a
    subtractS !a !s = let f d =  d - s in elementwiseScalarOp "subtract" f a
    multiplyS :: a -> Double -> a
    multiplyS !a !s = let f d = d * s in elementwiseScalarOp "multiply" f a
    divideS :: a -> Double -> a
    divideS !a !s = if s == 0.0
                    then traceStack "ZeroDivisionError :: performing zero division" a
                    else let f d = d / s in elementwiseScalarOp "divide" f a
