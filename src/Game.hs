module Game
    ( carParams
    , gameLoop
    , gameLoop' ) where

import qualified Car        as C
import           Data.IORef
import           Data.Maybe
import           Data.Word  (Word32)
import           SDL
import qualified SDL.Font   as TTF
import           SDL.Time
import           SDL.Vect   (V2, V4)
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
    , C._acceleration   = 8
    , C._deAcceleration = 6
    , C._turnSpeed      = 4
    , C._friction       = 0.6
    , C._turnFriction   = 0.6
    , C._color          = V4 100 100 255 255 }

fontSize :: TTF.PointSize
fontSize = 28

fontPath :: FilePath
fontPath = "fonts/VCR_OSD_MONO.ttf"

-- The time the player gets to play
gameTime :: Double
gameTime = 60

gameLoop :: (World -> IO ()) -> (World -> IO [Input]) ->
    Either (IORef Word32) Word32 -> World -> IO World
gameLoop drawFunc inputFunc time w = do
    drawFunc w
    input <- inputFunc w
    deltaTime <- case time of
                   Left gameTicks -> do
                       oldTick <- readIORef gameTicks
                       nowTick <- ticks
                       return $ fromIntegral (nowTick - oldTick) / 1000
                   Right delayTicks -> do
                       delay delayTicks
                       return $ fromIntegral delayTicks / 1000
    return $ worldTick input deltaTime w

gameLoop' :: (World -> [Input]) -> Word32 -> World -> World
gameLoop' inputFunc' delayTicks w =
    worldTick input deltaTime w

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
        maybeList  = [ if wHeld || upHeld    then Just GoForward
                                             else Nothing
                     , if aHeld || leftHeld  then Just GoLeft
                                             else Nothing
                     , if sHeld || downHeld  then Just GoBackward
                                             else Nothing
                     , if dHeld || rightHeld then Just GoRight
                                             else Nothing
                     , if qPressed || escPressed || quitRequested
                                             then Just Quit
                                             else Nothing]
    return $ catMaybes maybeList

isEventButtonPress :: Keycode -> Event -> Bool
isEventButtonPress code event =
            case eventPayload event of
              KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == code
              _ -> False
