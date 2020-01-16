module Graphics ( drawWorld ) where

import qualified Car                  as C
import           Control.Lens
import qualified Data.Vector.Storable as V
import           SDL                  hiding (rotate)
import           SDL.Vect             hiding (rotate)
import qualified Transform            as T
import           World

drawWorld :: Renderer -> World -> IO World
drawWorld ren world = do
    --     Draw the background
    rendererDrawColor ren $= V4 0 0 0 255
    clear ren
    --     Draw the car
    rendererDrawColor ren $= V4 0 255 0 255
    drawCar ren (view car world)
    --     Present the world
    present ren
    return world

rotate :: Floating a => a -> V2 a -> V2 a
rotate ang (V2 x y) = let x' = cos ang * x - sin ang * y
                          y' = sin ang * x + cos ang * y
                       in V2 x' y'

drawCar :: Renderer -> C.Car -> IO ()
drawCar ren car = do
    viewport <- get $ rendererViewport ren
    let w            = view (C.params . C.width) car / 2
        h            = view (C.params . C.height) car / 2
        c            = view C.position car
        rot          = view C.rotation car
        corners'     = [V2 w h, V2 (-w) h, V2 (-w) (-h), V2 w (-h), V2 w h]
    corners <- mapM (T.toScreenPoint ren . P . (c ^+^) . rotate rot) corners'
--    putStrLn $ "corners' \t" ++ show corners'
--    putStrLn $ "corners  \t" ++ show corners
    drawLines ren (V.fromList corners)
