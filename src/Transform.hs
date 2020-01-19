module Transform where

import           Foreign.C.Types
import           SDL
import           SDL.Vect

-- Units per width
upw :: RealFrac a => a
upw = 48

-- Units per height
uph :: RealFrac a => a
uph = 27

toScreenPoint :: RealFrac a => Renderer -> Point V2 a -> IO (Point V2 CInt)
toScreenPoint ren (P (V2 x0 y0)) = do
    viewport <- get $ rendererViewport ren
    let V2 w h = case viewport of
                   Just (Rectangle _ v) -> fromIntegral <$> v
                   Nothing              -> error "No viewport"
    let x1 = round $ x0 * (w / upw) + w / 2
        y1 = round $ h - (y0 * (h / uph) + h / 2)
    return (P (V2 x1 y1))

toScreenScale :: RealFrac a => Renderer -> V2 a -> IO (V2 CInt)
toScreenScale ren (V2 x0 y0) = do
    viewport <- get $ rendererViewport ren
    let V2 w h = case viewport of
                   Just (Rectangle _ v) -> fromIntegral <$> v
                   Nothing              -> error "No viewport"
    let x1 = round $ x0 * (w / upw)
        y1 = round $ y0 * (h / uph)
    return (V2 x1 y1)

toScreenRect :: RealFrac a => Renderer -> Rectangle a -> IO (Rectangle CInt)
toScreenRect ren (Rectangle c0 e0) = do
    c1 <- toScreenPoint ren c0
    e1 <- toScreenScale ren e0
    return (Rectangle c1 e1)

toWorldPoint :: RealFrac a => Renderer -> Point V2 CInt -> IO (Point V2 a)
toWorldPoint ren point = do
    viewport <- get $ rendererViewport ren
    let P (V2 x0 y0) = fromIntegral <$> point
    let V2 w h = case viewport of
                   Just (Rectangle _ v) -> fromIntegral <$> v
                   Nothing              -> error "No viewport"
    let x1 = (x0 - w / 2) * (upw / w)
        y1 = (y0 - h / 2) * (uph / h)
    return (P (V2 x1 y1))

toWorldScale :: RealFrac a => Renderer -> V2 CInt -> IO (V2 a)
toWorldScale ren vec = do
    viewport <- get $ rendererViewport ren
    let V2 x0 y0 = fromIntegral <$> vec
    let V2 w h = case viewport of
                   Just (Rectangle _ v) -> fromIntegral <$> v
                   Nothing              -> error "No viewport"
    let x1 = (w / 2) * (upw / w)
        y1 = (h / 2) * (uph / h)
    return (V2 x1 y1)

