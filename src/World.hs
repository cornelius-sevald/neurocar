{-# LANGUAGE TemplateHaskell #-}
module World
    ( World(..)
    , GameState(..)
    , Input(..)
    , gameState
    , car
    , track
    , score
    , timeLeft
    , initWorld
    , gameLoop
    ) where

import           Car
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Error.Util
import           Data.Word
import           Linear.V2
import           Track
import           Control.Applicative

data GameState = GameRunning
               | GameQuit
               | GameLost
               | TimeUp
               deriving (Eq, Ord, Enum, Show)

data World = World { _gameState :: GameState
                   , _car       :: Car
                   , _track     :: Track Double
                   , _score     :: Int
                   , _timeLeft  :: Double }
                   deriving (Eq, Show)

data Input = GoForward
           | GoBackward
           | GoLeft
           | GoRight
           | Quit
           deriving (Eq, Ord, Enum, Show)

makeLenses ''World

inputToCarActions :: [Input] -> [CarAction]
inputToCarActions []     = []
inputToCarActions (x:xs) = case x of
                             GoForward   -> CarAccelerate : inputToCarActions xs
                             GoLeft      -> CarTurnLeft : inputToCarActions xs
                             GoBackward  -> CarBreak : inputToCarActions xs
                             GoRight     -> CarTurnRight : inputToCarActions xs
                             _           -> inputToCarActions xs

initWorld :: CarParams -> V2 Double -> Double -> Track Double -> Double -> World
initWorld carParams carPos carRot track time = let car = newCar carParams carPos carRot
                                                in World { _gameState=GameRunning
                                                         , _car=car
                                                         , _track=track
                                                         , _score=0
                                                         , _timeLeft=time }

gameLoop :: [Input] -> Double -> World -> World
gameLoop inputs deltaTime world =
    let worldState = maybeT (return ()) return $ do
        { when (Quit `elem` inputs) (gameState .= GameQuit)
        ; guard $ view gameState world == GameRunning
        ; timeLeft -= deltaTime
        ; when (world^.timeLeft <= 0) $ (timeLeft .= 0 >> gameState .= TimeUp >> empty)
        ; let carInput = inputToCarActions inputs
        ; car %= execState (updateCar carInput deltaTime)
        ; let checkpointHit = carIntersectsCheckpoint (world^.track) (world^.car)
        ; when checkpointHit $ track.checkpointIndex += 1 >> score += 1
        ; let wallHit = carIntersects (view track world) (view car world)
        ; when wallHit (gameState .= GameLost >> empty) }
     in execState worldState world
