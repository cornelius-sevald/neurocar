module Main where

import           Control.Monad
import qualified Data.Text     as T
import           SDL

appLoop :: Renderer -> IO ()
appLoop renderer = do
    events <- pollEvents
    let eventIsQPress event =
            case eventPayload event of
              KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
              _ -> False
        qPressed = any eventIsQPress events
    rendererDrawColor renderer $= V4 0 0 255 255
    clear renderer
    present renderer
    unless qPressed (appLoop renderer)


main :: IO ()
main = do
    initializeAll
    window <- createWindow (T.pack "Neuro Car") defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer
