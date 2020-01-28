module AI.GeneticAlgorithm
    ( Genome
    , Individual
    , Population
    , individualGenome
    , individualFitness
    , mutate
    , proportionalSelection
    , getBasePopulation
    , evolve
    , evolves
    , bestAverageWorst ) where

import           AI.NeuralNetwork            as NN
import           Control.Lens
import           Control.Monad.State
import           Control.Parallel.Strategies
import           Data.Function
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Random.Normal
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as VG
import           Numeric.LinearAlgebra
import           System.Random

type Genome       = NN.Network
-- An individual with a genome and fitness
data Individual a = Individual Genome a
    deriving (Eq, Show)
type Population a = V.Vector (Individual a)

mut :: Double -> Double -> Double -> State StdGen Double
mut rate amount val = do
    rRate <- state random
    if rRate < rate
       then fmap (\x -> val + x * amount) (state normal)
       else return val

toIndividual :: (Genome -> a) -> Genome -> Individual a
toIndividual fitfunc genome = Individual genome (fitfunc genome)

toPopulation :: (Genome -> a) -> V.Vector Genome -> V.Vector (Individual a)
toPopulation fitfunc genomes = let pop = V.map (toIndividual fitfunc) genomes
                                in pop `using` parTraversable rseq

individualGenome :: Individual a -> Genome
individualGenome (Individual g _) = g

individualFitness :: Individual a -> a
individualFitness (Individual _ f) = f

mutate :: Double -> Double -> Genome -> State StdGen Genome
mutate rate amount genome = do
    let cmut = mut rate amount
    newBiases  <- mapM (VG.mapM    cmut) (genome^.biases)
    newWeights <- mapM (mapMatrixM cmut) (genome^.weights)
    let newNN = execState (biases .= newBiases >> weights .= newWeights) genome
    return newNN

proportionalSelection :: (Random a, Ord a, Num a, NFData a) =>
    Population a -> State StdGen (Population a)
proportionalSelection pop =
    let fitness  = individualFitness <$> pop
        accFit   = V.scanl1 (+) fitness
        choose p = snd $ fromJust $ V.find (\t -> p <= fst t) (V.zip accFit pop)
     in forM pop $ \_ -> do
         p <- state $ randomR (0, V.last accFit)
         return $ choose p

getBasePopulation :: (Genome -> a) -> State StdGen Genome ->
    Int -> State StdGen (Population a)
getBasePopulation fitfunc genomeGen popSize =
    toPopulation fitfunc <$> V.replicateM popSize genomeGen

evolve :: (Genome -> Double) -> (Genome -> State StdGen Genome) ->
    Population Double -> State StdGen (Population Double)
evolve fitfunc mutfunc pop = do
    selected <- proportionalSelection pop
    mutated  <- mapM (\(Individual g _) -> mutfunc g) selected
    return $ toPopulation fitfunc mutated

evolves :: Int -> Int -> State StdGen Genome ->
    (Genome -> Double) -> (Genome -> State StdGen Genome) ->
        State StdGen [Population Double]
evolves generations popSize genomeGen fitfunc mutfunc = do
    basePop <- getBasePopulation fitfunc genomeGen popSize
    let evofunc = evolve fitfunc mutfunc
    evolutions <- V.iterateNM generations evofunc basePop
    return $ V.toList evolutions

bestAverageWorst :: (Fractional a, Ord a) =>
    [Population a] -> [(Individual a, a, Individual a)]
bestAverageWorst = reverse . foldl (\acc pop ->
        let fitness = individualFitness <$> pop
            best = V.maximumBy (compare `on` individualFitness) pop
            avgFit = sum fitness / fromIntegral (V.length fitness)
            worst = V.minimumBy (compare `on` individualFitness) pop
         in (best, avgFit, worst) : acc) []

mapMatrixM :: (Element a, Element b, Monad m) =>
    (a -> m b) -> Matrix a -> m (Matrix b)
mapMatrixM f m = let ll  = toLists m
                     mmM = mapM . mapM
                  in fromLists <$> mmM f ll
