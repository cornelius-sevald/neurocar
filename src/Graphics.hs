module Graphics ( drawWorld ) where

import qualified Car                  as C
import           Control.Lens
import           Data.Text            (pack)
import           Data.Tuple.Extra     as TE
import qualified Data.Tuple.Sequence  as TS
import qualified Data.Vector.Storable as V
import           Geometry
import           SDL                  hiding (rotate)
import qualified SDL.Font             as TTF
import           SDL.Vect             hiding (rotate)
import qualified Track                as T
import qualified Transform            as F
import           World

white = V4 255 255 255 255

drawWorld :: Renderer -> TTF.Font -> World -> IO ()
drawWorld ren font world = do
    --     Draw the background
    rendererDrawColor ren $= V4 0 0 0 255
    clear ren
    --     Draw the car
    rendererDrawColor ren $= view (car . C.params . C.color) world
    drawCar ren (view car world)
    --     Draw the track
    --     If the car collides with the track,
    --     make the track red.
    case view gameState world of
      GameRunning -> rendererDrawColor ren $= view (track . T.color) world
      GameLost    -> rendererDrawColor ren $= V4 255   0 0 255
      TimeUp      -> rendererDrawColor ren $= V4 155 155 0 255
    drawTrack ren (view track world)
    --     Draw info to the player
    drawInfo ren font world
    --     Present the world
    present ren


drawCar :: Renderer -> C.Car -> IO ()
drawCar ren car = do
    corners <- mapM (F.toScreenPoint ren . P) (C.globalCorners car)
    let cornersC = last corners : corners
    drawLines ren (V.fromList cornersC)

drawTrack :: RealFrac a => Renderer -> T.Track a -> IO ()
drawTrack ren track = do
    let ringC' = last (view T.rings track) : view T.rings track
    innerC <- mapM ((F.toScreenPoint ren . P) . fst) ringC'
    outerC <- mapM ((F.toScreenPoint ren . P) . snd) ringC'
    drawLines ren (V.fromList innerC)
    drawLines ren (V.fromList outerC)
    -- Draw the checkpoint
    let checkpoint' = T.getCheckpoint track
    checkpoint <- TS.sequenceT $ TE.both (F.toScreenPoint ren . P) checkpoint'
    rendererDrawColor ren $= V4 255 255 255 255
    uncurry (drawLine ren) checkpoint

timeRect :: Rectangle Double
timeRect = let c = P (V2 (F.upw * 0.4) (F.uph * 0.5))
               e =    V2 (F.upw * 0.08) (F.uph * 0.06)
            in Rectangle c e

scoreRect :: Rectangle Double
scoreRect = let c = P (V2 (F.upw * 0.4) (F.uph * 0.425))
                e =    V2 (F.upw * 0.08) (F.uph * 0.06)
            in Rectangle c e

drawInfo :: Renderer -> TTF.Font -> World -> IO ()
drawInfo ren font world = do
    let timeText  = pack $ "Time:  " ++ show (round $ world^.timeLeft)
        scoreText = pack $ "Score: " ++ show (world^.score)
    timeSurf   <- TTF.solid font white timeText
    scoreSurf  <- TTF.solid font white scoreText
    timeTex    <- createTextureFromSurface ren timeSurf
    scoreTex   <- createTextureFromSurface ren scoreSurf
    _timeRect  <- F.toScreenRect ren timeRect
    _scoreRect <- F.toScreenRect ren scoreRect
    copy ren timeTex  Nothing (Just _timeRect)
    copy ren scoreTex Nothing (Just _scoreRect)
