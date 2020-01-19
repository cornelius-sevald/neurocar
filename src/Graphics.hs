module Graphics ( drawWorld ) where

import qualified Car                  as C
import           Control.Lens
import           Data.Tuple.Extra     as TE
import qualified Data.Tuple.Sequence  as TS
import qualified Data.Vector.Storable as V
import           Geometry
import           SDL                  hiding (rotate)
import           SDL.Vect             hiding (rotate)
import qualified Track                as T
import qualified Transform            as F
import           World

drawWorld :: Renderer -> World -> IO World
drawWorld ren world = do
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
    --     Present the world
    present ren
    return world


drawCar :: Renderer -> C.Car -> IO ()
drawCar ren car = do
    viewport <- get $ rendererViewport ren
    corners <- mapM (F.toScreenPoint ren . P) (C.globalCorners car)
    let cornersC = last corners : corners
    drawLines ren (V.fromList cornersC)

drawTrack :: RealFrac a => Renderer -> T.Track a -> IO ()
drawTrack ren track = do
    viewport <- get $ rendererViewport ren
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
