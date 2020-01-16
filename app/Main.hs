{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Car                 as C
import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Graphics
import           SDL
import           SDL.Time
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
    , C._color          = V4 255 0 0 255 }

main :: IO ()
main = do
    initializeAll
    window <- createWindow "Neuro Car" windowsConfig
    renderer <- createRenderer window (-1) rendererConfig
    let appLoop w = do { drawWorld renderer w
                       ; frameInfo <- getFrameInfo
                       ; return $ gameLoop frameInfo w }
     in void $ iterateUntilM (view shouldQuit) appLoop (initWorld carParams)

getFrameInfo :: IO FrameInfo
getFrameInfo = do
    t <- ticks
    events <- pollEvents
    kbs <- getKeyboardState
    return FrameInfo { _frameTicks = t
                     , _events = events
                     , _keyboardState = kbs }
