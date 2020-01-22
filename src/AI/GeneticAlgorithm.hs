{-# LANGUAGE TemplateHaskell #-}
module AI.GeneticAlgorithm
    ( mutate
    , proportionalSelection
    , getBasePopulation
    , evolve
    , evolves) where

import           AI.NeuralNetwork      as NN
import           Control.Lens
import           Control.Monad.State
import           Data.Random.Normal
import qualified Data.Vector           as V
import qualified Data.Vector.Generic   as VG
import           Numeric.LinearAlgebra
import           System.Random

type Genome = NN.Network
type Individual = Genome
type Population = [Individual]


mut :: Double -> Double -> Double -> State StdGen Double
mut rate amount val = do
    rRate <- state random
    if rRate < rate
       then fmap (\x -> val + x * amount) (state normal)
       else return val

mutate :: Double -> Double -> Individual -> State StdGen Individual
mutate rate amount nn = do
    let cmut = mut rate amount
    newBiases  <- mapM (VG.mapM    cmut) (nn^.biases)
    newWeights <- mapM (mapMatrixM cmut) (nn^.weights)
    let newNN = execState (biases .= newBiases >> weights .= newWeights) nn
    return newNN

proportionalSelection :: (Random a, Ord a, Num a) =>
    (Individual -> a) -> Population -> State StdGen Population
proportionalSelection fitfunc pop =
    let fit      = map fitfunc pop
        accFit   = scanl1 (+) fit
        choose p = snd $ head $ dropWhile (\t -> p > fst t) (zip accFit pop)
     in forM pop $ \_ -> do
         p <- state $ randomR (0, last accFit)
         return $ choose p

getBasePopulation :: State StdGen Individual -> Int -> State StdGen Population
getBasePopulation generator popSize = replicateM popSize generator

evolve :: (Individual -> Double) -> (Individual -> State StdGen Individual) ->
    Population -> State StdGen Population
evolve fitfunc mutfunc pop = do
    selected <- proportionalSelection fitfunc pop
    mapM mutfunc selected

evolves :: Int -> Int -> State StdGen Individual -> (Individual -> Double) ->
    (Individual -> State StdGen Individual) -> State StdGen [Population]
evolves generations popSize indGen fitfunc mutfunc = do
    basePop <- getBasePopulation indGen popSize
    let evofunc = evolve fitfunc mutfunc
    evolutions <- V.iterateNM generations evofunc basePop
    return $ V.toList evolutions

mapMatrixM :: (Element a, Element b, Monad m) =>
    (a -> m b) -> Matrix a -> m (Matrix b)
mapMatrixM f m = let ll  = toLists m
                     mmM = mapM . mapM
                  in fromLists <$> mmM f ll
