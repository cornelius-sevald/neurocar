{-# LANGUAGE TemplateHaskell #-}
module World
    ( World(..)
    , GameState(..)
    , Input(..)
    , gameState
    , car
    , initWorld
    , gameLoop
    ) where

import           Car
import           Control.Lens
import           Control.Monad.State
import           Data.Word

data GameState = GameRunning
               | GameQuit
               | GameLost
               deriving (Eq, Ord, Enum, Show)

data World = World { _gameState :: GameState
                   , _car       :: Car }
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

initWorld :: CarParams -> World
initWorld carParams = let car = newCar carParams
                          in World { _gameState = GameRunning
                                   , _car=car }

gameLoop :: [Input] -> Double -> World -> World
gameLoop inputs deltaTime world =
    let worldState = do
        { when (Quit `elem` inputs) (gameState .= GameQuit)
        ; when (view gameState world == GameRunning) $ do
            { let carInput = inputToCarActions inputs
            ; car %= execState (updateCar carInput deltaTime) } }
     in execState worldState world
