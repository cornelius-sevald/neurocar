{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Car                 as C
import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Data.IORef
import           Data.Word           (Word32)
import           Graphics
import           SDL
import           SDL.Time
import qualified Track               as T
import           World

windowsConfig :: WindowConfig
windowsConfig = WindowConfig
    { windowBorder          = True
    , windowHighDPI         = False
    , windowInputGrabbed    = False
    , windowMode            = Windowed
    , windowGraphicsContext = NoGraphicsContext
    , windowPosition        = Wherever
    , windowResizable       = True
    , windowInitialSize     = V2 800 450
    , windowVisible         = True }

rendererConfig :: RendererConfig
rendererConfig = RendererConfig
    { rendererType          = AcceleratedVSyncRenderer
    , rendererTargetTexture = False }


carParams :: C.CarParams
carParams = C.CarParams
    { C._width          = 0.65
    , C._height         = 1.0
    , C._acceleration   = 4
    , C._deAcceleration = 2
    , C._turnSpeed      = 2
    , C._friction       = 0.1
    , C._turnFriction   = 0.1
    , C._color          = V4 0 0 255 255 }


main :: IO ()
main = do
    initializeAll
    window <- createWindow "Neuro Car" windowsConfig
    renderer <- createRenderer window (-1) rendererConfig
    gameTrack <- T.fromCSV (V4 0 255 0 255) "tracks/track000.csv"
    let carPos = V2 (negate 20) (negate 10)
    let carRot = 0
    gameTicks <- newIORef (0 :: Word32)
    let appLoop w = do { drawWorld renderer w
                       ; input <- getUserInput
                       ; oldTick   <- readIORef gameTicks
                       ; nowTick <- ticks
                       ; let deltaTime = fromIntegral (nowTick - oldTick) / 1000
                       ; writeIORef gameTicks nowTick
                       ; return $ gameLoop input deltaTime w }
     in void $ iterateUntilM (\w -> view gameState w /= GameRunning) appLoop (initWorld carParams carPos carRot gameTrack)

boolList :: [(Bool, a)] -> [a]
boolList []              = []
boolList ((True, x):xs)  = x : boolList xs
boolList ((False, x):xs) = boolList xs

isEventButtonPress :: Keycode -> Event -> Bool
isEventButtonPress code event =
            case eventPayload event of
              KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == code
              _ -> False

getUserInput :: IO [Input]
getUserInput = do
    keyboardState  <- getKeyboardState
    events         <- pollEvents
    let eventIsQuitRequested event =
            case eventPayload event of
              QuitEvent -> True
              _         -> False
        quitRequested = any eventIsQuitRequested events
        wHeld      = keyboardState ScancodeW
        aHeld      = keyboardState ScancodeA
        sHeld      = keyboardState ScancodeS
        dHeld      = keyboardState ScancodeD
        upHeld     = keyboardState ScancodeUp
        leftHeld   = keyboardState ScancodeLeft
        downHeld   = keyboardState ScancodeDown
        rightHeld  = keyboardState ScancodeRight
        qPressed   = any (isEventButtonPress KeycodeQ) events
        escPressed = any (isEventButtonPress KeycodeEscape) events
        ifList     = [ (wHeld || upHeld,    GoForward)
                     , (aHeld || leftHeld,  GoLeft)
                     , (sHeld || downHeld,  GoBackward)
                     , (dHeld || rightHeld, GoRight)
                     , (qPressed || escPressed || quitRequested, Quit)]
    return $ boolList ifList
