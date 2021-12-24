-- euler angles logic
module Math3D.EulerAngles where

import Math3D.Vector

data EulerAngleType = YAW | PITCH | ROLL deriving(Eq, Show)

data EulerAngle = EuAngle {
        eulerVal :: Double,
        eulerHigh :: Double,
        eulerLow :: Double,
        eulerIsLimited :: Bool,
        angleType :: EulerAngleType
    } deriving(Eq, Show)

eulerValue :: EulerAngle -> Double
eulerValue eangle =
    let ehigh = eulerHigh eangle
        elow = eulerLow eangle
        evalue = eulerVal eangle
    in if eulerIsLimited eangle
       then if evalue < elow
            then elow
            else if evalue > ehigh
                 then ehigh
                 else evalue
       else evalue

data EulerAngles = EuAngles {
    eulerRoll :: EulerAngle,
    eulerYaw :: EulerAngle,
    eulerPitch :: EulerAngle
    } deriving(Eq, Show)

eulerToVec :: EulerAngles -> Vector
eulerToVec eangles =
    let roll = eulerValue $ eulerRoll eangles
        pitch = eulerValue $ eulerPitch eangles
        yaw = eulerValue $ eulerYaw eangles
    in fromList2Vec roll [pitch, yaw]


fromValsToEuler :: Double -> Double -> Double -> EulerAngles
fromValsToEuler roll pitch yaw =
    let rollAngle = EuAngle{ eulerVal = roll, 
                                eulerHigh = 0,
                                eulerLow = 0,
                                eulerIsLimited = False,
                                angleType = ROLL
                                }
        pitchAngle = EuAngle{ eulerVal = pitch, 
                                eulerHigh = 0,
                                eulerLow = 0,
                                eulerIsLimited = False,
                                angleType = PITCH
                                }
        yawAngle = EuAngle{ eulerVal = yaw,
                              eulerHigh = 0,
                              eulerLow = 0,
                              eulerIsLimited = False,
                              angleType = YAW}
    in EuAngles {eulerRoll = rollAngle,
                 eulerYaw = yawAngle,
                 eulerPitch = pitchAngle}


toFrontVec :: EulerAngles -> Vector
toFrontVec eangles =
    let yaw = eulerVal $ eulerYaw eangles
        pitch = eulerVal $ eulerPitch eangles
        f = (cos yaw) * (cos pitch)
    in fromList2Vec f [sin pitch, (sin yaw) * (cos pitch)]
