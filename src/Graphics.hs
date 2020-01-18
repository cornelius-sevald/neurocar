module Graphics ( drawWorld ) where

import qualified Car                  as C
import           Control.Lens
import qualified Data.Vector.Storable as V
import           Geometry
import           SDL                  hiding (rotate)
import           SDL.Vect             hiding (rotate)
import qualified Track                as T
import qualified Transform            as F
import           World

drawWorld :: Renderer -> World -> IO World
drawWorld ren world = do
    let wallHit = T.carIntersects (view track world) (view car world)
    --     Draw the background
    rendererDrawColor ren $= V4 0 0 0 255
    clear ren
    --     Draw the car
    rendererDrawColor ren $= view (car . C.params . C.color) world
    drawCar ren (view car world)
    --     Draw the track
    --     If the car collides with the track,
    --     make the track red.
    if wallHit
       then rendererDrawColor ren $= V4 255 0 0 255
       else rendererDrawColor ren $= view (track . T.color) world
    drawTrack ren (view track world)
    --     Present the world
    present ren
    return world


drawCar :: Renderer -> C.Car -> IO ()
drawCar ren car = do
    viewport <- get $ rendererViewport ren
    let w            = view (C.params . C.width) car / 2
        h            = view (C.params . C.height) car / 2
        c            = view C.position car
        rot          = view C.rotation car
        corners'     = [V2 w h, V2 (-w) h, V2 (-w) (-h), V2 w (-h), V2 w h]
    corners <- mapM (F.toScreenPoint ren . P . (c ^+^) . rotate rot) corners'
--    putStrLn $ "corners' \t" ++ show corners'
--    putStrLn $ "corners  \t" ++ show corners
    drawLines ren (V.fromList corners)

drawTrack :: RealFrac a => Renderer -> T.Track a -> IO ()
drawTrack ren track = do
    viewport <- get $ rendererViewport ren
    let outerC' = last (view T.outerRing track) : view T.outerRing track
        innerC' = last (view T.innerRing track) : view T.innerRing track
    outerC <- mapM (F.toScreenPoint ren . P) outerC'
    innerC <- mapM (F.toScreenPoint ren . P) innerC'
    drawLines ren (V.fromList outerC)
    drawLines ren (V.fromList innerC)
