{-# LANGUAGE TemplateHaskell #-}
module Car
    ( -- <Types>
      CarParams(..)
    , CarAction(..)
    , Car(..)
      -- </Types>
      -- <Lenses>
    , width
    , height
    , acceleration
    , deAcceleration
    , turnSpeed
    , friction
    , turnFriction
    , color
    , params
    , position
    , velocity
    , rotation
    , rotationVelocity
      -- </Lenses>
      -- <Util>
    , newCar
      -- </Util>
      -- <State>
    , moveCar
    , accelerateCar
    , rotateCar
    , rotAccelerateCar
    , updateCar
      -- </State>
    ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Function
import           Data.Word
import           SDL.Vect

data CarParams = CarParams { _width          :: Double
                           , _height         :: Double
                           , _acceleration   :: Double
                           , _deAcceleration :: Double
                           , _turnSpeed      :: Double
                           , _friction       :: Double
                           , _turnFriction   :: Double
                           , _color          :: V4 Word8 }
                           deriving (Eq, Show, Read)

data CarAction
    = CarAccelerate
    | CarBreak
    | CarTurnLeft
    | CarTurnRight
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Car = Car { _params           :: CarParams
               , _position         :: V2 Double
               , _velocity         :: V2 Double
               , _rotation         :: Double
               , _rotationVelocity :: Double }
               deriving (Eq, Show, Read)

makeLenses ''CarParams
makeLenses ''Car

newCar :: CarParams -> V2 Double -> Double -> Car
newCar params startPos startRot = Car { _params = params
                                      , _position = startPos
                                      , _velocity = pure 0
                                      , _rotation = startRot + pi
                                      , _rotationVelocity = 0 }

moveCar :: V2 Double -> State Car ()
moveCar amount = state $ \car -> ((), over position (+ amount) car)

accelerateCar :: V2 Double -> State Car ()
accelerateCar amount = state $ \car -> ((), over velocity (+ amount) car)

rotateCar :: Double -> State Car ()
rotateCar amount = state $ \car -> ((), over rotation (+ amount) car)

rotAccelerateCar :: Double -> State Car ()
rotAccelerateCar amount = state $ \car -> ((), over rotationVelocity (+ amount) car)


updateCar :: Foldable f => f CarAction -> Double -> State Car ()
updateCar actions deltaTime = do
    let (accelerate, break, left, right) = carActionsToBools actions
    car <- get
    let dir     = angle $ view rotation                 car - pi / 2
    let accel   = dir ^* view (params . acceleration)   car ^* deltaTime
    let deAccel = dir ^* view (params . deAcceleration) car ^* deltaTime
    let rot     = view (params . turnSpeed)             car  * deltaTime
    when accelerate (accelerateCar accel)
    when break      (accelerateCar $ negate deAccel)
    when right      (rotAccelerateCar (-rot))
    when left       (rotAccelerateCar rot)
    get >>= moveCar   . (^* deltaTime) . view velocity
    get >>= rotateCar . ( * deltaTime) . view rotationVelocity
    -- Friction not implemented


carActionsToBools :: Foldable f => f CarAction -> (Bool, Bool, Bool, Bool)
carActionsToBools = foldl (\(a, b, l, r) action ->
    case action of
      CarAccelerate -> if b then (False, False, l ,r)
                            else (True , False, l, r)
      CarBreak      -> if a then (False, False, l, r)
                            else (False, True , l, r)
      CarTurnLeft   -> if r then (a, b, False, False)
                            else (a, b, True , False)
      CarTurnRight  -> if l then (a, b, False, False)
                            else (a, b, False, True )) (False, False, False, False)
