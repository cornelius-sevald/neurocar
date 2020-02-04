module Graphics
    ( white
    , black
    , red
    , green
    , blue
    , renderText ) where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Data.Word              (Word8)
import           Foreign.C.Types        (CInt)
import           SDL
import qualified SDL.Font               as TTF
import           SDL.Vect

white = V4 255 255 255 255 :: V4 Word8
black = V4 000 000 000 255 :: V4 Word8
red   = V4 255 000 000 255 :: V4 Word8
green = V4 000 255 000 255 :: V4 Word8
blue  = V4 000 000 255 255 :: V4 Word8

renderText :: MonadIO m => Renderer -> TTF.Font -> V4 Word8 -> Rectangle CInt -> Text -> m ()
renderText ren font color rect contents = do
    textSurf <- TTF.solid font color contents
    textTex  <- createTextureFromSurface ren textSurf

    rendererDrawColor ren $= color
    copy ren textTex  Nothing (Just rect)

    freeSurface textSurf
    destroyTexture textTex
