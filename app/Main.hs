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
import qualified SDL.Font as TTF
import           System.IO

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
    , C._acceleration   = 8
    , C._deAcceleration = 6
    , C._turnSpeed      = 4
    , C._friction       = 0.6
    , C._turnFriction   = 0.6
    , C._color          = V4 0 0 255 255 }

fontSize :: TTF.PointSize
fontSize = 28

fontPath :: FilePath
fontPath = "fonts/VCR_OSD_MONO.ttf"

-- The time the player gets to play
gameTime :: Double
gameTime = 60

main :: IO ()
main = do
    initializeAll
    TTF.initialize
    window <- createWindow "Neuro Car" windowsConfig
    renderer <- createRenderer window (-1) rendererConfig
    font <- TTF.load fontPath fontSize
    gameTrack <- T.fromFile (V4 0 255 0 255) "tracks/track001.nct"
    let carPos = V2 (negate 20) (negate 10)
    let carRot = 0
    gameTicks <- newIORef (0 :: Word32)
    let appLoop w = do
        { drawWorld renderer font w
        ; input <- getUserInput
        ; oldTick   <- readIORef gameTicks
        ; nowTick <- ticks
        ; let deltaTime = fromIntegral (nowTick - oldTick) / 1000
        ; writeIORef gameTicks nowTick
        ; return $ gameLoop input deltaTime w }
     in void $ iterateUntilM (\w -> view gameState w == GameQuit) appLoop (initWorld carParams gameTrack gameTime)

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
