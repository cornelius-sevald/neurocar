{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Car                 as C
import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Graphics
import           SDL
import           World

main :: IO ()
main = do
    initializeAll
    window <- createWindow "Neuro Car" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    let carParams = C.CarParams { C._width=0.65
                                , C._height=1.0
                                , C._acceleration=4
                                , C._deAcceleration=2
                                , C._turnSpeed=2
                                , C._friction=0.1
                                , C._turnFriction=0.1
                                , C._color=V4 255 0 0 255 }
    let appLoop w = drawWorld renderer w >> gameLoop w
     in void $ iterateUntilM (view shouldQuit) appLoop (initWorld carParams)
