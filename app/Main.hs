{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           AI.GeneticAlgorithm   as GA
import           AI.NeuralNetwork      as NN
import qualified Car                   as C
import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Data.Foldable
import           Data.Function
import           Data.Functor.Identity
import qualified Data.Vector           as V
import           Data.Word             (Word32)
import           Geometry              as G
import           Graphics
import qualified Numeric.LinearAlgebra as LA
import           SDL
import qualified SDL.Font              as TTF
import           SDL.Time
import           System.IO
import           System.Random
import           Text.Printf
import qualified Track                 as T
import           Util
import           World


seed :: Int
seed = 27

main :: IO ()
main = do
    -- Game track
    gameTrack <- T.fromFile (V4 0 255 0 255) "tracks/track000.nct"

    -- App loop
    let runGame nn = runIdentity $ iterateUntilM (\w -> view gameState w /= GameRunning)
            (Identity . aiLoop nn) (initWorld carParams gameTrack gameTime)

    -- Evolution

    let baw = bestAverageWorst evolutions
    printf "GEN \t MIN \t AVG \t MAX\n"
    forM_ (zip ([0..] :: [Int]) baw) $ \(n, (b, a, w)) -> do
        let minFit = individualFitness w
        let avgFit = a
        let maxFit = individualFitness b
        printf "%d \t %.2f \t %.2f \t %.2f\n" n minFit avgFit maxFit
    let bestIndividual = fst3 $ maximumBy (compare `on` individualFitness . fst3) baw
    NN.toFile ("cars/bestcar-" ++ show seed ++ ".nn") (individualGenome bestIndividual)
