module Graphics where

import qualified Car                  as C
import           Control.Lens
import qualified Data.Vector.Storable as V
import           SDL                  hiding (rotate)
import           SDL.Vect             hiding (rotate)

rotate :: Floating a => V2 a -> a -> V2 a
rotate (V2 x y) ang = let x' = cos ang * x - sin ang * y
                          y' = sin ang * x + cos ang * y
                       in V2 x' y'

drawCar :: Renderer -> C.Car -> IO ()
drawCar ren car = do
    viewport <- get $ rendererViewport ren
    let screenCenter = case viewport of
                         Just (Rectangle _ v) -> (fromIntegral <$> v) ^/ 2
        center       = view C.position car + screenCenter
        rot          = view C.rotation car
        w            = view (C.params . C.width) car / 2
        h            = view (C.params . C.height) car / 2
        corners'     = [V2 w h, V2 (-w) h, V2 (-w) (-h), V2 w (-h), V2 w h]
        corners      = V.fromList $ map (\v -> P . fmap round $ rotate v rot ^+^ center) corners'
    drawLines ren corners
