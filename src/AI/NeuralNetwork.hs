{-# LANGUAGE TemplateHaskell #-}
module AI.NeuralNetwork
    ( Network
    , layerSizes
    , biases
    , weights
    , sigmoid
    , newNetwork
    , feedforward ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Random.Normal
import           Numeric.LinearAlgebra
import           System.Random

data Network = Network { _layerSizes :: [Int]
                       , _biases     :: [Vector Double]
                       , _weights    :: [Matrix Double] }
                       deriving (Eq, Show)

makeLenses ''Network

sigmoid :: Floating a => a -> a
sigmoid z = 1.0 / (1 + exp (negate z))

newNetwork :: [Int] -> State StdGen Network
newNetwork sizes = do
    let matrixSizes = zip (init sizes) (tail sizes)

    biasesL  <- mapM sequence
        [[state normal | _ <- [1..y]] | y <- tail sizes]
    weightsL <- mapM sequence $
        [[state normal | _ <- [1..x], x <- [1..y]] | (x, y) <- matrixSizes]

    let randBiases  = map fromList biasesL
    let randWeights = map (\((x, y), l) -> (y><x) l) (zip matrixSizes weightsL)

    return Network { _layerSizes = sizes
                   , _biases = randBiases
                   , _weights = randWeights }

feedforward :: Vector Double -> Network -> Vector Double
feedforward input nn = foldl (\a (b, w) -> cmap sigmoid ((w #> a) + b)) input bw
    where bw = zip (nn^.biases) (nn^.weights)
