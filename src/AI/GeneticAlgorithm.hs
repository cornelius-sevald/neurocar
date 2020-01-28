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
    , getBasePopulationM
    , evolveM
    , evolvesM
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

mut :: Monad m => Double -> Double -> Double -> StateT StdGen m Double
mut rate amount val = do
    rRate <- state random
    if rRate < rate
       then fmap (\x -> val + x * amount) (state normal)
       else return val

toIndividualM :: Monad m => (Genome -> m a) -> Genome -> m (Individual a)
toIndividualM fitfunc genome = do
    fitness <- fitfunc genome
    return $ Individual genome fitness

toIndividual fitfunc = runIdentity . toIndividualM (Identity . fitfunc)

toPopulationM :: Monad m => (Genome -> m a) ->
    V.Vector Genome -> m (V.Vector (Individual a))
toPopulationM fitfunc genomes = do
    pop <- V.mapM (toIndividualM fitfunc) genomes
    return (pop `using` parTraversable rseq)

toPopulation fitfunc = runIdentity . toPopulationM (Identity . fitfunc)

individualGenome :: Individual a -> Genome
individualGenome (Individual g _) = g

individualFitness :: Individual a -> a
individualFitness (Individual _ f) = f

mutate :: Monad m => Double -> Double -> Genome -> StateT StdGen m Genome
mutate rate amount genome = do
    let cmut = mut rate amount
    newBiases  <- mapM (VG.mapM    cmut) (genome^.biases)
    newWeights <- mapM (mapMatrixM cmut) (genome^.weights)
    let newNN = execState (biases .= newBiases >> weights .= newWeights) genome
    return newNN

proportionalSelection :: (Random a, Ord a, Num a, NFData a, Monad m) =>
    Population a -> StateT StdGen m (Population a)
proportionalSelection pop =
    let fitness  = individualFitness <$> pop
        accFit   = V.scanl1 (+) fitness
        choose p = snd $ fromJust $ V.find (\t -> p <= fst t) (V.zip accFit pop)
     in forM pop $ \_ -> do
         p <- state $ randomR (0, V.last accFit)
         return $ choose p

getBasePopulationM :: Monad m => (Genome -> m a) -> StateT StdGen m Genome ->
    Int -> StateT StdGen m (Population a)
getBasePopulationM fitfunc genomeGen popSize = do
    genomes <- V.replicateM popSize genomeGen
    lift $ toPopulationM fitfunc genomes

getBasePopulation fitfunc = getBasePopulationM (Identity . fitfunc)

evolveM :: Monad m => (Genome -> m Double) -> (Genome -> StateT StdGen m Genome) ->
    Population Double -> StateT StdGen m (Population Double)
evolveM fitfunc mutfunc pop = do
    selected <- proportionalSelection pop
    mutated  <- mapM (\(Individual g _) -> mutfunc g) selected
    lift $ toPopulationM fitfunc mutated

evolve fitfunc = evolveM (Identity . fitfunc)

evolvesM :: Monad m => Int -> Int -> StateT StdGen m Genome ->
    (Genome -> m Double) -> (Genome -> StateT StdGen m Genome) ->
        StateT StdGen m [Population Double]
evolvesM generations popSize genomeGen fitfunc mutfunc = do
    basePop <- getBasePopulationM fitfunc genomeGen popSize
    let evofunc = evolveM fitfunc mutfunc
    evolutions <- V.iterateNM generations evofunc basePop
    return $ V.toList evolutions

evolves generations popSize genomeGen fitfunc =
    evolvesM generations popSize genomeGen (Identity . fitfunc)

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
