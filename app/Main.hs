module Main where

import qualified AI.GeneticAlgorithm   as GA
import qualified AI.NeuralNetwork      as NN
import qualified AI.NeuroCar           as NC
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Word             (Word32)
import           Game
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Printf
import qualified Track                 as T
import           Util

data Flags
    = Generate  -- -g, --gen
    | Play      -- -p, --play
    | Help      -- -h, --help
    deriving (Eq, Ord, Enum, Show, Bounded)

tracksPath = "tracks/"

carsPath = "cars/"

flags =
    [ Option ['g'] ["gen"] (NoArg Generate)
            "Train a new car."
    , Option ['p'] ["play"] (NoArg Play)
            "Play the game, alone or against a neural car. Overrides '--gen'."
    , Option ['h'] ["help"] (NoArg Help)
            "Print this help message and exit the program."
    ]

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

prompt :: Read a => String -> IO a
prompt str = putStr str >> hFlush stdout >> readLn

promptStr :: String -> IO String
promptStr str = putStr str >> hFlush stdout >> getLine

main :: IO ()
main = do
    progName <- getProgName
    as <- getArgs >>= parse progName
    when (Play `elem` as) (playGame >> exitSuccess)
    when (Generate `elem` as) (generateCar >> exitSuccess)
    playGame
    exitSuccess

playGame :: IO ()
playGame = putStrLn "video gaming"

generateCar :: IO ()
generateCar = do
    seed        <- prompt    "Seed: "              :: IO Int
    genCount    <- prompt    "Generations: "       :: IO Int
    popSize     <- prompt    "Population size: "   :: IO Int
    mutChance   <- prompt    "Mutation chance: "   :: IO Double
    mutStrength <- prompt    "Mutation strength: " :: IO Double
    deltaTicks  <- prompt    "Delta ticks: "       :: IO Word32
    time        <- prompt    "Training time: "     :: IO Double
    trackName   <- promptStr "Track name: "        :: IO String
    track       <- T.fromFile $ tracksPath ++ trackName ++ ".nct"
    evolutions  <- NC.evolveCar seed genCount popSize mutChance mutStrength time deltaTicks carParams track
    let baw = GA.bestAverageWorst evolutions
    putStrLn "ruth"
    forM_ (zip ([0..] :: [Int]) baw) $ \(n, (b, a, w)) -> do
        let minFit = GA.individualFitness w
        let avgFit = a
        let maxFit = GA.individualFitness b
        printf "%d \t %.2f \t %.2f \t %.2f\n" n minFit avgFit maxFit
    let bestIndividual = fst3 $ maximumBy (compare `on` GA.individualFitness . fst3) baw
    let carName = "car-" ++ show seed
    NN.toFile (carsPath ++ carName ++ ".nn") (GA.individualGenome bestIndividual)
