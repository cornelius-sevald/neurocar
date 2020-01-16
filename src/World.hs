{-# LANGUAGE TemplateHaskell #-}
module World
    ( World(..)
    , shouldQuit
    , gameTicks
    , getCar
    , initWorld
    , gameLoop
    ) where

import           Car
import           Control.Lens
import           Control.Monad.State
import           Data.Word
import           SDL

data World = World { _shouldQuit :: Bool
                   , _gameTicks  :: Word32
                   , _getCar     :: Car }

makeLenses ''World

isEventButtonPress :: Keycode -> Event -> Bool
isEventButtonPress code event =
            case eventPayload event of
              KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == code
              _ -> False

initWorld :: CarParams -> World
initWorld carParams = let car = newCar carParams
                       in World { _shouldQuit=False
                                , _gameTicks=0
                                , _getCar=car }

gameLoop :: World -> IO World
gameLoop world = do
    -- Time
    tickCount <- ticks
    let prevTickCount = view gameTicks world
        deltaTime = fromIntegral (tickCount - prevTickCount) / 1000
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
        ((), car') = runState (updateCar carInput deltaTime) (view getCar world)
    let world' = set getCar car' world
    let world'' = if qPressed then set shouldQuit True world'
                            else world'
    let world''' = set gameTicks tickCount world''
    -- Wait
    return world'''
