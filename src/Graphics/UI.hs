{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Graphics.UI
    ( ButtonState(..)
    , TextFieldState(..)
    , Button(..)
    , TextField(..)
    , Label(..)
    , UI(..)
    -- Lenses
    , buttonSize
    , buttonPos
    , buttonColors
    , buttonTextSize
    , buttonText
    , buttonFont
    , buttonState
    , buttonAction
    , fieldSize
    , fieldPos
    , fieldColors
    , fieldTextSize
    , fieldText
    , fieldFont
    , fieldState
    , labelSize
    , labelPos
    , labelText
    , labelFont
    , labelColor
    -- Helper
    , modifyButtonState
    , modifyFieldState
    -- Logic
    , uiLoop
    ) where

import           Control.Concurrent.MVar
import           Control.Error.Util
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Data.Function             (on)
import qualified Data.Map                  as Map
import qualified Data.Text                 as Text
import qualified Data.Tuple.Extra          as Tuple
import qualified Data.Vector.Storable      as V
import           Data.Word                 (Word8)
import           Foreign.C.Types
import           Geometry
import           SDL
import qualified SDL.Font                  as TTF
import           SDL.Raw.Types             (Rect (..))
import           SDL.Vect
import           SDL.Video.Renderer

-- Classes

class UIElement a where
    size       :: a -> V2 Double
    position   :: a -> Point V2 Double

class (UIElement a) => UITextElement a where
    textSize     :: a -> V2 Double
    textPosition :: a -> Point V2 Double
    textFont     :: a -> TTF.Font
    textColor    :: a -> V4 Word8
    text         :: a -> Text.Text

-- Types

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
             , _buttonColors   :: [(V4 Word8, V4 Word8)]
             , _buttonTextSize :: V2 Double
             , _buttonText     :: Text.Text
             , _buttonFont     :: TTF.Font
             , _buttonState    :: ButtonState
             , _buttonAction   :: StateT Button IO () }

data TextField
    = TextField { _fieldSize     :: V2 Double
                , _fieldPos      :: Point V2 Double
                , _fieldColors   :: [(V4 Word8, V4 Word8)]
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

data UI = UI (Map.Map String (MVar Button   ))
             (Map.Map String (MVar TextField))
             (Map.Map String (MVar Label    ))

-- Instances

instance UIElement Button where
    size       = _buttonSize
    position   = _buttonPos

instance UIElement TextField where
    size       = _fieldSize
    position   = _fieldPos

instance UIElement Label where
    size       = _labelSize
    position   = _labelPos

instance UITextElement Button where
    textSize     = _buttonTextSize
    textPosition = _buttonPos
    textFont     = _buttonFont
    textColor b  = fst $ _buttonColors b !! fromEnum (_buttonState b)
    text         = _buttonText

instance UITextElement TextField where
    textSize     = _fieldTextSize
    textPosition = _fieldPos
    textFont     = _fieldFont
    textColor f  = fst $ _fieldColors f !! fromEnum (_fieldState f)
    text         = _fieldText

instance UITextElement Label where
    textSize     = _labelSize
    textPosition = _labelPos
    textFont     = _labelFont
    textColor    = _labelColor
    text         = _labelText

-- Lenses

makeLenses ''Button
makeLenses ''TextField
makeLenses ''Label

-- Functions

modifyButtonState :: Button -> ButtonState -> Button
modifyButtonState button newState = flip execState button $ buttonState .= newState

modifyFieldState :: TextField -> TextFieldState -> TextField
modifyFieldState field newState = flip execState field $ fieldState .= newState

toScreenScale :: RealFrac a => V2 CInt -> V2 a -> V2 CInt
toScreenScale (V2 w h) (V2 x y) = V2 (round $ x * fromIntegral w)
                                     (round $ y * fromIntegral h)

toScreenPos :: RealFrac a => V2 CInt -> Point V2 a -> Point V2 CInt
toScreenPos (V2 w h) (P (V2 x y)) = P $ V2 (round $ x * fromIntegral w)
                                           (round $ y * fromIntegral h)

screenRectFromUIElem :: UIElement a => V2 CInt -> a -> Rectangle CInt
screenRectFromUIElem wp uiElem =
    let extents  = toScreenScale wp (size uiElem)
        ulCorner = toScreenPos   wp (position uiElem - P (size uiElem / 2))
     in Rectangle ulCorner extents

mouseOverUIElem :: UIElement a => V2 CInt -> a -> IO Bool
mouseOverUIElem wp uiElem = do
    mousePos            <- getAbsoluteMouseLocation
    let mousePos'       = fromIntegral <$> mousePos
    let rect'           = screenRectFromUIElem wp uiElem
    let rect            = fromIntegral <$> rect'
    return $ pointInRect rect mousePos'

eventIsM1Motion :: InputMotion -> Event -> Bool
eventIsM1Motion motion event =
    case eventPayload event of
      MouseButtonEvent mouseButtonEvent ->
          mouseButtonEventMotion mouseButtonEvent == motion &&
              mouseButtonEventButton mouseButtonEvent == ButtonLeft
      _ -> False

drawFillRect :: Renderer -> Rectangle CInt -> (V4 Word8, V4 Word8) -> IO ()
drawFillRect ren rect colors = do
    rendererDrawColor ren $= snd colors
    fillRect ren (Just rect)
    rendererDrawColor ren $= fst colors
    drawRect ren (Just rect)

drawUIText :: UITextElement a => V2 CInt -> Renderer -> a -> IO ()
drawUIText wp ren textElem = maybeT (return ()) return $ do
    let center   = toScreenPos wp $ textPosition textElem
    let contents = text textElem
    let font     = textFont textElem
    let color    = textColor textElem
    guard $ contents /= ""
    (utw, uth) <- Tuple.both fromIntegral <$> TTF.size font contents
    let height = textSize textElem ^. _y / 2
    let width' = textSize textElem ^. _x / 2
    let width  = min width' (utw / uth * height)
    let size   = toScreenScale wp (V2 width height)
    let textCenter = let x = round $ fromIntegral (size^._x) / 2.2
                         y = round $ fromIntegral (size^._y) / 1.8 -- magic numbers 4head
                      in center - P (V2 x y)
    let textRect   = Rectangle textCenter size

    textSurf <- TTF.solid font color contents
    textTex  <- createTextureFromSurface ren textSurf

    rendererDrawColor ren $= color
    copy ren textTex  Nothing (Just textRect)

drawTextField :: V2 CInt -> Renderer -> TextField -> IO ()
drawTextField wp ren field = do
    let rect        = screenRectFromUIElem wp field
    let colors      = (field^.fieldColors) !! fromEnum (field^.fieldState)
    drawFillRect ren rect colors
    drawUIText wp ren field

drawButton :: V2 CInt -> Renderer -> Button -> IO ()
drawButton wp ren button = do
    let rect        = screenRectFromUIElem wp button
    let colors      = (button^.buttonColors) !! fromEnum (button^.buttonState)
    drawFillRect ren rect colors
    drawUIText wp ren button

drawLabel :: V2 CInt -> Renderer -> Label -> IO ()
drawLabel = drawUIText

updateButton :: V2 CInt -> [Event] -> MVar Button -> IO ()
updateButton wp events mButton = do
    button    <- takeMVar mButton
    newButton <- flip execStateT button $ do
        mouseOverButton <- lift $ mouseOverUIElem wp button
        let state       = button^.buttonState

        let m1IsPressed  = any (eventIsM1Motion Pressed)  events
            m1IsReleased = any (eventIsM1Motion Released) events

        when (state == ButtonPressable &&
              mouseOverButton) $ buttonState .= ButtonHovered
        when (state == ButtonHovered &&
              not mouseOverButton) $ buttonState .= ButtonPressable
        when (state == ButtonHovered &&
              m1IsPressed) $ buttonState .= ButtonPressed
        when (state == ButtonPressed &&
              m1IsReleased) $ buttonState .= ButtonPressable >>
                              when mouseOverButton (button^.buttonAction)
    putMVar mButton newButton

updateField :: V2 CInt -> [Event] -> MVar TextField -> IO ()
updateField wp events mField = do
    field    <- takeMVar mField
    newField <- flip execStateT field $ do
        mouseOverButton <- lift $ mouseOverUIElem wp field
        let state           = field^.fieldState

        let m1IsPressed  = any (eventIsM1Motion Pressed)  events

        when (state == FieldTypable &&
              mouseOverButton) $ fieldState .= FieldHovered
        when (state == FieldHovered &&
              not mouseOverButton) $ fieldState .= FieldTypable
        when (state == FieldHovered &&
              m1IsPressed) $ fieldState .= FieldTyping >>
                             let rect = (\(Rectangle (P (V2 x y)) (V2 w h)) ->
                                        Rect x y w h) (screenRectFromUIElem wp field)
                              in startTextInput rect
        when (state == FieldTyping &&
              m1IsPressed) $ fieldState .= FieldTypable >>
                             stopTextInput

        when (state == FieldTyping) $ forM_ events $ \event -> do
            contents <- gets _fieldText
            case eventPayload event of
              TextInputEvent inputEvent   -> fieldText %= flip Text.append (textInputEventText inputEvent)
              KeyboardEvent keyboardEvent ->
                  when (keyboardEventKeyMotion keyboardEvent == Pressed) $
                        case keysymKeycode (keyboardEventKeysym keyboardEvent) of
                          KeycodeBackspace -> unless (Text.null contents) $ fieldText %= Text.init
                          KeycodeDelete    -> fieldText .= ""
                          KeycodeEscape    -> fieldState .= FieldTypable
                          KeycodeReturn    -> fieldState .= FieldTypable
                          _                -> return ()
              _                           -> return ()
    putMVar mField newField

updateUI :: V2 CInt -> [Event] -> UI -> IO ()
updateUI wp events (UI buttons fields labels) = do
    mapM_ (updateButton wp events) buttons
    mapM_ (updateField wp events)  fields

drawUI :: V2 CInt -> Renderer -> UI -> IO ()
drawUI wp ren (UI buttons fields labels) = do
    --     Draw the background
    rendererDrawColor ren $= V4 0 0 0 255

    clear ren
    mapM_ (readMVar >=> drawButton wp ren)    buttons
    mapM_ (readMVar >=> drawTextField wp ren) fields
    mapM_ (readMVar >=> drawLabel wp ren)     labels

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

    updateUI viewport events ui
    drawUI viewport ren ui
    unless qPressed (uiLoop ren ui)
