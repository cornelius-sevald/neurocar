{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified AI.GeneticAlgorithm        as GA
import qualified AI.NeuralNetwork           as NN
import qualified AI.NeuroCar                as NC
import           Control.Concurrent.MVar
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Function
import           Data.List
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Text                  as Text
import           Data.Word                  (Word32)
import           Game
import qualified Graphics
import qualified Graphics.UI                as UI
import qualified SDL
import qualified SDL.Font                   as TTF
import           SDL.Vect
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Random
import           Text.Printf
import qualified Text.Read                  as R
import qualified Track                      as T
import           Util

data Flags
    = Generate  -- -g, --gen
    | Play      -- -p, --play
    | Help      -- -h, --help
    deriving (Eq, Ord, Enum, Show, Bounded)

tracksPath = "tracks/"

carsPath = "cars/"

fontSize :: TTF.PointSize
fontSize = 28

fontPath :: FilePath
fontPath = "fonts/VCR_OSD_MONO.ttf"

windowsConfig :: SDL.WindowConfig
windowsConfig = SDL.WindowConfig
    { SDL.windowBorder          = True
    , SDL.windowHighDPI         = False
    , SDL.windowInputGrabbed    = False
    , SDL.windowMode            = SDL.Windowed
    , SDL.windowGraphicsContext = SDL.NoGraphicsContext
    , SDL.windowPosition        = SDL.Wherever
    , SDL.windowResizable       = True
    , SDL.windowInitialSize     = V2 800 450
    , SDL.windowVisible         = True }

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
    { SDL.rendererType          = SDL.AcceleratedVSyncRenderer
    , SDL.rendererTargetTexture = False }


flags =
    [ Option ['g'] ["gen"] (NoArg Generate)
            "Train a new car."
    , Option ['p'] ["play"] (NoArg Play)
            "Play the game, alone or against a neural car. Overrides '--gen'."
    , Option ['h'] ["help"] (NoArg Help)
            "Print this help message and exit the program."
    ]


main :: IO ()
main = do
    progName <- getProgName
    as <- getArgs >>= parse progName
    when (Play `elem` as) (playGame >> exitSuccess)
    when (Generate `elem` as) (generateCar >> exitSuccess)
    playGame
    exitSuccess

playGame :: IO ()
playGame = do
    SDL.initializeAll
    TTF.initialize

    window   <- SDL.createWindow (Text.pack "neuro  car") windowsConfig
    renderer <- SDL.createRenderer window (-1) rendererConfig
    ui       <- setupUI
    UI.uiLoop renderer ui

    TTF.quit
    SDL.quit

generateCar :: IO ()
generateCar = exceptT putStrLn
    return $ do
        randSeed    <- flip mod 1000 <$> liftIO (randomIO :: IO Int)
        seed        <- prompt "Seed" (Just randSeed)                                  :: ExceptT String IO Int
        genCount    <- boundedPrompt "Generations" (Just 20) (1, 10000)               :: ExceptT String IO Int
        popSize     <- boundedPrompt "Population size" (Just 20) (1, 10000)           :: ExceptT String IO Int
        mutChance   <- boundedPrompt "Mutation chance" (Just 0.1) (0, 1)              :: ExceptT String IO Double
        mutStrength <- boundedPrompt "Mutation strength" (Just 1) (0, 100)            :: ExceptT String IO Double
        time        <- boundedPrompt "Training time" (Just 60) (0, 3600)              :: ExceptT String IO Double
        deltaTicks  <- boundedPrompt "Delta ticks" (Just 40) (1, round $ time * 1000) :: ExceptT String IO Word32
        trackName   <- prompt "Track name" Nothing                                    :: ExceptT String IO String
        let trackPath = trackNameToPath trackName
        unlessM (liftIO $ doesFileExist trackPath) $ throwE "Invalid track name"
        track       <- liftIO $ T.fromFile trackPath
        let evolutions = NC.evolveCar seed genCount popSize mutChance mutStrength time deltaTicks carParams track
        let baw = GA.bestAverageWorst evolutions
        forM_ (zip ([0..] :: [Int]) baw) $ \(n, (b, a, w)) -> do
            let minFit = GA.individualFitness w
            let avgFit = a
            let maxFit = GA.individualFitness b
            liftIO $ printf "%d \t %.2f \t %.2f \t %.2f\n" n minFit avgFit maxFit
        let bestIndividual = fst3 $ maximumBy (compare `on` GA.individualFitness . fst3) baw
        let carName = "car-" ++ show seed
        liftIO $ createDirectoryIfMissing True carsPath
        liftIO $ NN.toFile (carNameToPath carName) (GA.individualGenome bestIndividual)


trackNameToPath :: String -> String
trackNameToPath name = tracksPath ++ name ++ ".nct"

carNameToPath :: String -> String
carNameToPath name = carsPath ++ name ++ ".nn"

parse :: String -> [String] -> IO [Flags]
parse progName argv = case getOpt Permute flags argv of
               (args,fs,[]) ->
                   if Help `elem` args
                      then do hPutStrLn stderr (usageInfo header flags)
                              exitSuccess
                      else return (nub args)
               (_,_,errs)      -> do
                   hPutStrLn stderr (concat errs ++ usageInfo header flags)
                   exitWith (ExitFailure 1)
               where header = "Usage: " ++ progName ++ " [-gph]"

prompt :: (Show a, Read a, MonadIO m) =>
    String -> Maybe a -> ExceptT String m a
prompt str defaultVal = do
    let str' = case defaultVal of
                 Just val -> str ++ " [" ++ show val ++ "]: "
                 Nothing  -> str ++ ": "
    line <- liftIO $ putStr str' >>
                   hFlush stdout >>
                   getLine
    if null line
       then case defaultVal of
              Just val -> return val
              Nothing  -> throwE "Empty input"
       else case R.readMaybe line of
              Just val -> return val
              Nothing  -> hoistEither $ note "Invalid input"
                                       (R.readMaybe $ "\"" ++ line ++ "\"")

boundedPrompt  :: (Ord a, Show a, Read a, MonadIO m) =>
    String -> Maybe a -> (a, a) -> ExceptT String m a
boundedPrompt str defaultVal bound = do
    readValue <- prompt str defaultVal
    if readValue < fst bound || snd bound < readValue
       then throwE $ "Value '" ++ show readValue ++
           "' not in range [" ++ show (fst bound) ++ " : " ++
               show (snd bound) ++ "]"
       else return readValue

setupUI :: IO UI.UI
setupUI = do
    font <- TTF.load fontPath fontSize
    trackField <- newMVar
            UI.TextField { UI._fieldSize     = V2 0.6 0.1
                         , UI._fieldPos      = P $ V2 0.44 0.4
                         , UI._fieldColors   = [ (Graphics.green,     Graphics.black)
                                               , (V4 100 100 100 255, Graphics.black)
                                               , (Graphics.black,     Graphics.green)
                                               , (Graphics.black,     V4 000 200 000 255)
                                               ]
                         , UI._fieldTextSize = V2 0.8 0.1
                         , UI._fieldText     = ""
                         , UI._fieldFont     = font
                         , UI._fieldState    = UI.FieldTypable }
    carField <- newMVar
            UI.TextField { UI._fieldSize     = V2 0.6 0.1
                         , UI._fieldPos      = P $ V2 0.44 0.6
                         , UI._fieldColors   = [ (Graphics.green,     Graphics.black)
                                               , (V4 100 100 100 255, Graphics.black)
                                               , (Graphics.black,     Graphics.green)
                                               , (Graphics.black,     V4 000 200 000 255)
                                               ]
                         , UI._fieldTextSize = V2 0.8 0.1
                         , UI._fieldText     = ""
                         , UI._fieldFont     = font
                         , UI._fieldState    = UI.FieldUnTypable }
    timeField <- newMVar
            UI.TextField { UI._fieldSize     = V2 0.1 0.1
                         , UI._fieldPos      = P $ V2 0.81 0.4
                         , UI._fieldColors   = [ (Graphics.green,     Graphics.black)
                                               , (V4 100 100 100 255, Graphics.black)
                                               , (Graphics.black,     Graphics.green)
                                               , (Graphics.black,     V4 000 200 000 255)
                                               ]
                         , UI._fieldTextSize = V2 0.15 0.1
                         , UI._fieldText     = "60"
                         , UI._fieldFont     = font
                         , UI._fieldState    = UI.FieldTypable }
    fpsField <- newMVar
            UI.TextField { UI._fieldSize     = V2 0.1 0.1
                         , UI._fieldPos      = P $ V2 0.81 0.6
                         , UI._fieldColors   = [ (Graphics.green,     Graphics.black)
                                               , (V4 100 100 100 255, Graphics.black)
                                               , (Graphics.black,     Graphics.green)
                                               , (Graphics.black,     V4 000 200 000 255)
                                               ]
                         , UI._fieldTextSize = V2 0.15 0.1
                         , UI._fieldText     = "24"
                         , UI._fieldFont     = font
                         , UI._fieldState    = UI.FieldUnTypable }
    playButton <- newMVar
            UI.Button { UI._buttonSize     = V2 0.2 0.1
                      , UI._buttonPos      = P $ V2 0.5 0.8
                      , UI._buttonColors   = [ (Graphics.green,     Graphics.black)
                                             , (V4 100 100 100 255, Graphics.black)
                                             , (Graphics.black,     Graphics.green)
                                             , (Graphics.black,     V4 000 200 000 255)
                                             ]
                      , UI._buttonTextSize = V2 0.3 0.2
                      , UI._buttonText     = "PLAY!"
                      , UI._buttonFont     = font
                      , UI._buttonState    = UI.ButtonPressable
                      , UI._buttonAction   = state return}
    carButton <- newMVar
            UI.Button { UI._buttonSize     = V2 0.05625 0.1
                      , UI._buttonPos      = P $ V2 0.1 0.6
                      , UI._buttonColors   = [ (Graphics.green,     Graphics.black)
                                             , (V4 100 100 100 255, Graphics.black)
                                             , (Graphics.black,     Graphics.green)
                                             , (Graphics.black,     V4 000 200 000 255)
                                             ]
                      , UI._buttonTextSize = V2 0.1 0.1
                      , UI._buttonText     = ""
                      , UI._buttonFont     = font
                      , UI._buttonState    = UI.ButtonPressable
                      , UI._buttonAction   = do { text <- use UI.buttonText
                                                ; if text == "X"
                                                     then do
                                                         UI.buttonText .= ""
                                                         lift $ modifyMVar carField (\f -> return (UI.modifyFieldState f UI.FieldUnTypable, ()))
                                                     else do
                                                         UI.buttonText .= "X"
                                                         lift $ modifyMVar carField (\f -> return (UI.modifyFieldState f UI.FieldTypable, ())) } }
    fpsButton <- newMVar
            UI.Button { UI._buttonSize     = V2 0.05625 0.1
                      , UI._buttonPos      = P $ V2 0.9 0.6
                      , UI._buttonColors   = [ (Graphics.green,     Graphics.black)
                                             , (V4 100 100 100 255, Graphics.black)
                                             , (Graphics.black,     Graphics.green)
                                             , (Graphics.black,     V4 000 200 000 255)
                                             ]
                      , UI._buttonTextSize = V2 0.1 0.1
                      , UI._buttonText     = ""
                      , UI._buttonFont     = font
                      , UI._buttonState    = UI.ButtonPressable
                      , UI._buttonAction   = do { text <- use UI.buttonText
                                                ; if text == "X"
                                                     then do
                                                         UI.buttonText .= ""
                                                         lift $ modifyMVar fpsField (\f -> return (UI.modifyFieldState f UI.FieldUnTypable, ()))
                                                     else do
                                                         UI.buttonText .= "X"
                                                         lift $ modifyMVar fpsField (\f -> return (UI.modifyFieldState f UI.FieldTypable, ())) } }
    trackLabel <- newMVar
            UI.Label { UI._labelSize  = V2 0.3 0.1
                     , UI._labelPos   = P $ V2 0.44 0.31
                     , UI._labelColor = Graphics.white
                     , UI._labelText  = "Track name"
                     , UI._labelFont  = font }
    carLabel <- newMVar
            UI.Label { UI._labelSize  = V2 0.3 0.1
                     , UI._labelPos   = P $ V2 0.44 0.51
                     , UI._labelColor = Graphics.white
                     , UI._labelText  = Text.pack "Car name"
                     , UI._labelFont  = font }
    timeLabel <- newMVar
            UI.Label { UI._labelSize  = V2 0.15 0.1
                     , UI._labelPos   = P $ V2 0.81 0.31
                     , UI._labelColor = Graphics.white
                     , UI._labelText  = Text.pack "Time"
                     , UI._labelFont  = font }
    fpsLabel <- newMVar
            UI.Label { UI._labelSize  = V2 0.1 0.1
                     , UI._labelPos   = P $ V2 0.81 0.51
                     , UI._labelColor = Graphics.white
                     , UI._labelText  = Text.pack "FPS"
                     , UI._labelFont  = font }

    return $ UI.UI (Map.fromList [ ("play button", playButton)
                                 , ("car button" , carButton)
                                 , ("fps button" , fpsButton)
                                 ])
                   (Map.fromList [ ("track field", trackField)
                                 , ("car field"  , carField)
                                 , ("time field" , timeField)
                                 , ("fps field"  , fpsField)
                                 ])
                   (Map.fromList [ ("track label", trackLabel)
                                 , ("car label"  , carLabel)
                                 , ("time label" , timeLabel)
                                 , ("fps label"  , fpsLabel)
                                 ])
