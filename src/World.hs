{-# LANGUAGE TemplateHaskell #-}
module World
    ( World(..)
    , FrameInfo(..)
    , shouldQuit
    , worldTicks
    , car
    , events
    , keyboardState
    , frameTicks
    , initWorld
    , gameLoop
    ) where

import           Car
import           Control.Lens
import           Control.Monad.State
import           Data.Word
import           SDL                 hiding (ticks)

data World = World { _shouldQuit :: Bool
                   , _worldTicks :: Word32
                   , _car        :: Car }

data FrameInfo = FrameInfo { _events        :: [Event]
                           , _keyboardState :: Scancode -> Bool
                           , _frameTicks    :: Word32 }

makeLenses ''World
makeLenses ''FrameInfo

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
                                , _worldTicks=0
                                , _car=car }

gameLoop :: FrameInfo -> World -> World
gameLoop info world = let fTicks = view frameTicks info
                          wTicks = view worldTicks world
                          deltaTime = fromIntegral (fTicks - wTicks) / 1000
                          -- Input
                          pressed code = any (isEventButtonPress code) (view events info)
                          held = view keyboardState info
                          qPressed = pressed KeycodeQ
                          wHeld = held ScancodeW
                          aHeld = held ScancodeA
                          sHeld = held ScancodeS
                          dHeld = held ScancodeD
                          -- Game logic
                          buttonActions = zip [wHeld, sHeld, aHeld, dHeld] [minBound .. maxBound]
                          carInput = foldl (\acc (b, i) -> if b then i:acc else acc) [] buttonActions
                       in execState (do { car %= execState (updateCar carInput deltaTime)
                                        ; when qPressed (shouldQuit .= True)
                                        ; worldTicks .= view frameTicks info }) world
