{-# LANGUAGE TemplateHaskell #-}
module UI
    ( ButtonState(..)
    , TextFieldState(..)
    , Button(..)
    , TextField(..)
    , Label(..)
    , UI(..)
    ) where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Vector.Storable as V
import           Data.Word            (Word8)
import           Foreign.C.Types
import           SDL
import qualified SDL.Font             as TTF
import           SDL.Vect

data ButtonState
    = Pressable
    | UnPressable
    | ButtonHovered
    | Pressed
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data TextFieldState
    = Typable
    | UnTypable
    | FieldHovered
    | Typing
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Button
    = Button { _buttonSize     :: V2 Double
             , _buttonPos      :: Point V2 Double
             , _buttonColors   :: [V4 Word8]
             , _buttonTextSize :: V2 Double
             , _buttonText     :: String
             , _buttonState    :: ButtonState
             , _buttonAction   :: State Button (IO ()) }

data TextField
    = TextField { _fieldSize     :: V2 Double
                , _fieldPos      :: Point V2 Double
                , _fieldColors   :: [V4 Word8]
                , _fieldTextSize :: V2 Double
                , _fieldText     :: String
                , _fieldState    :: TextFieldState }

data Label
    = Label { _labelSize  :: V2 Double
            , _labelPos   :: Point V2 Double
            , _labelColor :: V4 Word8 }

data UI = UI [Button] [TextField] [Label]

makeLenses ''Button
makeLenses ''TextField
makeLenses ''Label

toScreenScale :: RealFrac a => V2 CInt -> V2 a -> V2 CInt
toScreenScale (V2 w h) (V2 x y) = V2 (round $ x * fromIntegral w)
                                     (round $ y * fromIntegral h)

toScreenPos :: RealFrac a => V2 CInt -> Point V2 a -> Point V2 CInt
toScreenPos (V2 w h) (P (V2 x y)) = P $ V2 (round $ x * fromIntegral w)
                                           (round $ y * fromIntegral h)

drawButton :: V2 CInt -> Renderer -> Button -> IO ()
drawButton wp ren button = do
    let corners' = toScreenScale wp <$> [ button^.buttonSize
                                        , over _x negate (button^.buttonSize)
                                        , over _y negate (button^.buttonSize)
                                        , negated $ button^.buttonSize
                                        , button^.buttonSize ]
    let center = toScreenPos wp (button^.buttonPos)
    let corners = map (\v -> P $ v ^+^ unP center) corners'

    let color = (button^.buttonColors) !! fromEnum (button^.buttonState)
    rendererDrawColor ren $= color
    drawLines ren (V.fromList corners)
    return ()

