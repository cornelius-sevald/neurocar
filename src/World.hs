{-# LANGUAGE TemplateHaskell #-}
module World
    ( World(..)
    , GameState(..)
    , Input(..)
    , gameState
    , car
    , track
    , initWorld
    , gameLoop
    ) where

import           Car
import           Control.Lens
import           Control.Monad.State
import           Data.Word
import           Linear.V2
import           Track

data GameState = GameRunning
               | GameQuit
               | GameLost
               deriving (Eq, Ord, Enum, Show)

data World = World { _gameState :: GameState
                   , _car       :: Car
                   , _track     :: Track Double }
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

initWorld :: CarParams -> V2 Double -> Double -> Track Double -> World
initWorld carParams carPos carRot track = let car = newCar carParams carPos carRot
                                           in World { _gameState=GameRunning
                                                    , _car=car
                                                    , _track=track }

gameLoop :: [Input] -> Double -> World -> World
gameLoop inputs deltaTime world =
    let worldState = do when (Quit `elem` inputs) (gameState .= GameQuit)
                        when (view gameState world == GameRunning) $ car %= execState (updateCar carInput deltaTime)
                            where carInput = inputToCarActions inputs
     in execState worldState world
