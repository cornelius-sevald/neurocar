{-# LANGUAGE TemplateHaskell #-}
module UI
    ( ButtonState(..)
    , TextFieldState(..)
    , Button(..)
    , TextField(..)
    , Label(..)
    , UI(..)
    , uiLoop
    ) where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Text            as Text
import qualified Data.Tuple.Extra     as Tuple
import qualified Data.Vector.Storable as V
import           Data.Word            (Word8)
import           Foreign.C.Types
import           SDL
import qualified SDL.Font             as TTF
import           SDL.Vect

data ButtonState
    = ButtonPressable
    | ButtonUnPressable
    | ButtonHovered
    | ButtonPressed
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data TextFieldState
    = FieldTypable
    | FieldUnTypable
    | FieldHovered
    | FieldTyping
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Button
    = Button { _buttonSize     :: V2 Double
             , _buttonPos      :: Point V2 Double
             , _buttonColors   :: [V4 Word8]
             , _buttonTextSize :: V2 Double
             , _buttonText     :: Text.Text
             , _buttonFont     :: TTF.Font
             , _buttonState    :: ButtonState
             , _buttonAction   :: State Button (IO ()) }

data TextField
    = TextField { _fieldSize     :: V2 Double
                , _fieldPos      :: Point V2 Double
                , _fieldColors   :: [V4 Word8]
                , _fieldTextSize :: V2 Double
                , _fieldText     :: Text.Text
                , _fieldFont     :: TTF.Font
                , _fieldState    :: TextFieldState }

data Label
    = Label { _labelSize  :: V2 Double
            , _labelPos   :: Point V2 Double
            , _labelText  :: Text.Text
            , _labelFont  :: TTF.Font
            , _labelColor :: V4 Word8 }

data UI = UI [Button] [TextField] [Label]

makeLenses ''Button
makeLenses ''TextField
makeLenses ''Label

toScreenScale :: RealFrac a => V2 CInt -> V2 a -> V2 CInt
toScreenScale (V2 w h) (V2 x y) = V2 (round $ x * fromIntegral w / 2)
                                     (round $ y * fromIntegral h / 2)

toScreenPos :: RealFrac a => V2 CInt -> Point V2 a -> Point V2 CInt
toScreenPos (V2 w h) (P (V2 x y)) = P $ V2 (round $ x * fromIntegral w)
                                           (round $ y * fromIntegral h)

drawLabel :: V2 CInt -> Renderer -> Label -> IO ()
drawLabel wp ren label = do
    let center = toScreenPos wp $ label^.labelPos
    let color = label^.labelColor
    let text = label^.labelText
    let font = label^.labelFont
    (utw, uth) <- Tuple.both fromIntegral <$> TTF.size font text
    let textHeight = label^.labelSize._y
    let textWidth' = label^.labelSize._x
    let textWidth  = min textWidth' (utw / uth * textHeight)
    let textSize   = toScreenScale wp (V2 textWidth textHeight)
    let textCenter = let x = round $ fromIntegral (textSize^._x) / 2.2
                         y = round $ fromIntegral (textSize^._y) / 1.8 -- magic numbers 4head
                      in center - P (V2 x y)
    let textRect   = Rectangle textCenter textSize

    textSurf <- TTF.solid font color text
    textTex  <- createTextureFromSurface ren textSurf

    rendererDrawColor ren $= color
    copy ren textTex  Nothing (Just textRect)


drawButton :: V2 CInt -> Renderer -> Button -> IO ()
drawButton wp ren button = do
    let corners' = toScreenScale wp <$>
            [ button^.buttonSize
            , over _x negate (button^.buttonSize)
            , negated $ button^.buttonSize
            , over _y negate (button^.buttonSize)
            , button^.buttonSize ]
    let center      = toScreenPos wp (button^.buttonPos)
    let corners     = map (\v -> P $ v ^+^ unP center) corners'
    let color = (button^.buttonColors) !! fromEnum (button^.buttonState)
    let label = Label { _labelSize  = button^.buttonTextSize
                      , _labelPos   = button^.buttonPos
                      , _labelText  = button^.buttonText
                      , _labelFont  = button^.buttonFont
                      , _labelColor = color }

    rendererDrawColor ren $= color
    drawLines ren (V.fromList corners)
    drawLabel wp ren label

drawUI :: V2 CInt -> Renderer -> UI -> IO ()
drawUI wp ren (UI buttons _ labels) = do
    --     Draw the background
    rendererDrawColor ren $= V4 0 0 0 255

    clear ren
    mapM_ (drawButton wp ren) buttons
    mapM_ (drawLabel wp ren)  labels

    present ren

uiLoop :: Renderer -> UI -> IO ()
uiLoop ren ui = do
    events <- pollEvents
    let eventIsQPress event = case eventPayload event of
                                KeyboardEvent keyboardEvent ->
                                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                                        keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                                _ -> False
    let qPressed = any eventIsQPress events
    viewport'    <- SDL.get $ rendererViewport ren
    let viewport = (\(Just (Rectangle _ v)) -> v) viewport'
    drawUI viewport ren ui
    unless qPressed (uiLoop ren ui)
