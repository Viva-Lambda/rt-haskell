-- type for holding physical quantities
module Physics.Quantity where

import Math3D.CommonOps
import Math3D.Vector

import Debug.Trace

data PhysicalQuantity = ScalarField Double String 
                      | VectorField Vector String
                      deriving (Eq, Show)

unit :: PhysicalQuantity -> String
unit a = case a of
            ScalarField _ b -> b
            VectorField _ b -> b

isCompatible :: PhysicalQuantity -> PhysicalQuantity -> Bool
isCompatible a b = 
    if (unit a) == (unit b)
    then case a of
            ScalarField _ _ -> case b of
                                    ScalarField _ _ -> True
                                    VectorField _ _ -> False
            VectorField _ _ -> case b of
                                    ScalarField _ _ -> False
                                    VectorField _ _ -> True
    else False


emptyQuantity :: PhysicalQuantity
emptyQuantity = ScalarField 0.0 ""


physicalOp :: String -> (Double -> Double -> Double) -> PhysicalQuantity -> PhysicalQuantity -> PhysicalQuantity
physicalOp str f a b =
    let errmsg = let msg = "physical quantities" ++ (show a) ++ " " ++ (show b)
                     msg2 = msg ++ " are not compatible in operation " ++ str
                 in msg2
    in if isCompatible a b
       then case a of
                ScalarField aval _ -> 
                    case b of
                        ScalarField bval _ -> ScalarField (f aval bval) (unit a)
                        VectorField _ _ -> traceStack errmsg emptyQuantity
                VectorField aval _ ->
                    case b of
                        ScalarField _ _ -> traceStack errmsg emptyQuantity
                        VectorField bval _ ->
                            VectorField (vecArithmeticOp str f aval bval) (unit a)
       else traceStack errmsg emptyQuantity


physicalScalarOp :: (Double -> Double) -> PhysicalQuantity -> PhysicalQuantity

physicalScalarOp f a =
    case a of
        ScalarField b s -> ScalarField (f b) s
        VectorField b s -> VectorField (vecScalarOp f b) s


instance BinaryOps PhysicalQuantity where
    
    elementwiseOp str f a b = physicalOp str f a b

    elementwiseScalarOp _ f a = physicalScalarOp f a

    divide a b =
        let errmsg = let msg = "ZeroDivisionError :: dividing physical " 
                         msg2 = "quantity contains zero "
                         msg3 = msg ++ msg2 ++ (show b)
                     in traceStack msg3 emptyQuantity
        in case b of
            ScalarField bval _ -> 
                if bval == 0.0
                then errmsg
                else elementwiseOp "divide" (/) a b
            VectorField bval _ -> 
                let es = vec2List bval
                in if 0.0 `elem` es
                   then errmsg
                   else elementwiseOp "divide" (/) a b

