module Game
    ( carParams
    , getUserInput
    , gameLoop
    , gameLoop'
    , eventIsButtonPress ) where

import qualified Car           as C
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import           Data.Word     (Word32)
import           SDL
import           SDL.Time
import           SDL.Vect      (V2, V4)
import           System.Exit
import           Util
import           World

carParams :: C.CarParams
carParams = C.CarParams
    { C._width          = 0.65
    , C._height         = 1.0
    , C._acceleration   = 8
    , C._deAcceleration = 6
    , C._turnSpeed      = 4
    , C._friction       = 0.6
    , C._turnFriction   = 0.6
    , C._color          = V4 100 100 255 255 }

-- The time the player gets to play
gameTime :: Double
gameTime = 60

gameLoop :: (World -> IO ()) -> (World -> IO [Input]) ->
    IO Word32 -> World -> IO World
gameLoop drawFunc inputFunc timefunc w = do
    drawFunc w
    input <- inputFunc w
    deltaTime <- (\x -> fromIntegral x / 1000) <$> timefunc
    return $ worldTick input deltaTime w

gameLoop' :: (World -> [Input]) -> Word32 -> World -> World
gameLoop' inputFunc delayTicks w = worldTick input deltaTime w
    where input = inputFunc w
          deltaTime = fromIntegral delayTicks / 1000

getUserInput :: IO [Input]
getUserInput = do
    keyboardState  <- getKeyboardState
    events         <- pollEvents

    let eventIsQuit event = eventPayload event == QuitEvent
    when (any eventIsQuit events) exitSuccess

    let wHeld      = keyboardState ScancodeW
        aHeld      = keyboardState ScancodeA
        sHeld      = keyboardState ScancodeS
        dHeld      = keyboardState ScancodeD
        upHeld     = keyboardState ScancodeUp
        leftHeld   = keyboardState ScancodeLeft
        downHeld   = keyboardState ScancodeDown
        rightHeld  = keyboardState ScancodeRight
        rPressed   = any (eventIsButtonPress KeycodeR) events
        qPressed   = any (eventIsButtonPress KeycodeQ) events
        escPressed = any (eventIsButtonPress KeycodeEscape) events
        maybeList  = [ if wHeld || upHeld    then Just GoForward
                                             else Nothing
                     , if aHeld || leftHeld  then Just GoLeft
                                             else Nothing
                     , if sHeld || downHeld  then Just GoBackward
                                             else Nothing
                     , if dHeld || rightHeld then Just GoRight
                                             else Nothing
                     , if rPressed           then Just Restart
                                             else Nothing
                     , if qPressed || escPressed
                                             then Just Quit
                                             else Nothing]
    return $ catMaybes maybeList
