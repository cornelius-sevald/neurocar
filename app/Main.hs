{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           AI.GeneticAlgorithm   as GA
import           AI.NeuralNetwork      as NN
import qualified Car                   as C
import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Data.Foldable
import           Data.Function
import           Data.Functor.Identity
import           Data.IORef
import qualified Data.Vector           as V
import           Data.Word             (Word32)
import           Geometry              as G
import           Graphics
import qualified Numeric.LinearAlgebra as LA
import           SDL
import qualified SDL.Font              as TTF
import           SDL.Time
import           System.IO
import           System.Random
import           Text.Printf
import qualified Track                 as T
import           Util
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

aiRayAngle :: Double
aiRayAngle = pi

aiRayCount :: Int
aiRayCount = 7

seed :: Int
seed = 22

main :: IO ()
main = do
    -- Game track
    gameTrack <- T.fromFile (V4 0 255 0 255) "tracks/track001.nct"

    -- App loop
    let runGame nn = runIdentity $ iterateUntilM (\w -> view gameState w /= GameRunning)
            (Identity . aiLoop nn) (initWorld carParams gameTrack gameTime)

    -- Evolution
    let gen = mkStdGen seed
    let fitfunc = fromIntegral . view score . runGame
    let mutfunc = GA.mutate 0.1 1
    let indGen = NN.newNetwork [3+aiRayCount, 15, 15, 2]
    let generations = 100
    let popSize = 120
    let (evolutions, gen') = runState (evolves generations popSize indGen fitfunc mutfunc) gen
    let baw = bestAverageWorst evolutions
    printf "GEN \t MIN \t AVG \t MAX\n"
    forM_ (zip ([0..] :: [Int]) baw) $ \(n, (b, a, w)) -> do
        let minFit = individualFitness w
        let avgFit = a
        let maxFit = individualFitness b
        printf "%d \t %.2f \t %.2f \t %.2f\n" n minFit avgFit maxFit
    let bestIndividual = fst3 $ maximumBy (compare `on` individualFitness . fst3) baw
    NN.toFile ("cars/bestcar-" ++ show seed ++ ".nn") (individualGenome bestIndividual)

playerLoop :: Renderer -> TTF.Font -> IORef Word32 -> World -> IO World
playerLoop ren font gameTicks w = do
    drawWorld ren font w
    input <- getUserInput
    oldTick <- readIORef gameTicks
    nowTick <- ticks
    let deltaTime = fromIntegral (nowTick - oldTick) / 1000
    writeIORef gameTicks nowTick
    return $ worldTick input deltaTime w

aiLoop :: NN.Network -> World -> World
aiLoop nn w =
    let input = getNetworkInput nn w
        deltaTime = 1/24
     in worldTick input deltaTime w


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

getNetworkInput :: NN.Network -> World -> [Input]
getNetworkInput nn w =
    let push a        = (state $ \xs -> ((),a:xs)) :: State [Input] ()
        rays          = C.shootRays aiRayAngle aiRayCount $ w^.car
        origin        = w^.car.C.position
        intersections = T.rayIntersection (w^.track) <$> rays
        distances     = (\case Nothing -> 0
                               Just v  -> G.segLength (origin, v)) <$> intersections
        velocity      = C.localVelocity $ w^.car
        rotation      = w^.car.C.rotation
        x             = rotation : toList velocity ++ distances
        y             = LA.toList $ NN.feedforward (LA.fromList x) nn
     in execState (do { when (head y < 1/3) (push GoBackward)
                      ; when (head y > 2/3) (push GoForward)
                      ; when (y !! 1 < 1/3) (push GoLeft)
                      ; when (y !! 1 > 2/3) (push GoRight) }) []

bestAverageWorst :: (Fractional a, Ord a) =>
    [GA.Population a] -> [(GA.Individual a, a, GA.Individual a)]
bestAverageWorst = reverse . foldl (\acc pop ->
        let fitness = individualFitness <$> pop
            best = V.maximumBy (compare `on` individualFitness) pop
            avgFit = sum fitness / fromIntegral (V.length fitness)
            worst = V.minimumBy (compare `on` individualFitness) pop
         in (best, avgFit, worst) : acc) []
