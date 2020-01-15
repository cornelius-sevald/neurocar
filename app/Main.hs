{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Car
import           Control.Monad
import           Control.Monad.State
import           Graphics
import           SDL

isEventButtonPress :: Keycode -> Event -> Bool
isEventButtonPress code event =
            case eventPayload event of
              KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == code
              _ -> False

appLoop :: Renderer -> Car -> IO ()
appLoop renderer car = do
    -- Events
    events <- pollEvents
    keyboardState <- getKeyboardState
    let qPressed = any (isEventButtonPress KeycodeQ) events
        wHeld = keyboardState ScancodeW
        aHeld = keyboardState ScancodeA
        sHeld = keyboardState ScancodeS
        dHeld = keyboardState ScancodeD
    -- Game logic
    let buttonActions = zip [wHeld, sHeld, aHeld, dHeld] [minBound .. maxBound]
        carInput = foldl (\acc (b, i) -> if b then i:acc else acc) [] buttonActions
        ((), car') = runState (updateCar carInput 0.0002) car
    -- Drawing
    --     Draw the background
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    --     Draw the car
    rendererDrawColor renderer $= V4 0 255 0 255
    drawCar renderer car
    present renderer
    unless qPressed (appLoop renderer car')


main :: IO ()
main = do
    initializeAll
    window <- createWindow "Neuro Car" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    let car = newCar $ CarParams { _width=35
                                 , _height=50
                                 , _acceleration=5
                                 , _deAcceleration=3
                                 , _turnSpeed=0.08
                                 , _friction=0.1
                                 , _turnFriction=0.1
                                 , _color=V4 255 0 0 255 }
    appLoop renderer car
