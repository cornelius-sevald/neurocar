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
    , localCorners
    , globalCorners
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
import qualified Geometry            as G
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

-- Calculate the friction multiplier from the
-- friction coefficient and delta time.
-- This is not how it actualy works in the real world.
calcFrictionMultiplier :: Floating a => a -> a -> a
calcFrictionMultiplier mu dt = (1 / (1-mu))**(-dt)


newCar :: CarParams -> V2 Double -> Double -> Car
newCar params startPos startRot = Car { _params = params
                                      , _position = startPos
                                      , _velocity = pure 0
                                      , _rotation = startRot + pi
                                      , _rotationVelocity = 0 }

localCorners :: Car -> [V2 Double]
localCorners car = let w = view (params . width) car / 2
                       h = view (params . height) car / 2
                    in [V2 w h, V2 (-w) h, V2 (-w) (-h), V2 w (-h)]

globalCorners :: Car -> [V2 Double]
globalCorners car = let corners' = localCorners car
                        c        = view position car
                        rot      = view rotation car
                     in map ((c ^+^) . G.rotate rot) corners'

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
    -- Frcitoon
    let moveMu  = view (params . friction)              car
    let rotMu   = view (params . friction)              car
    let moveFricMul = calcFrictionMultiplier moveMu deltaTime
    let rotFricMul  = calcFrictionMultiplier moveMu deltaTime
    velocity._x      *= moveFricMul
    velocity._y      *= moveFricMul
    rotationVelocity *= rotFricMul
    -- Movement
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
