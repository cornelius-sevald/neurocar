{-# LANGUAGE TemplateHaskell #-}
module AI.NeuralNetwork
    ( Network
    , layerSizes
    , biases
    , weights
    , sigmoid
    , newNetwork
    , feedforward
    , toFile
    , fromFile ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Random.Normal
import           Numeric.LinearAlgebra
import           System.IO
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

toFile :: FilePath -> Network -> IO ()
toFile filepath nn =
    let headerStr  = show (nn^.layerSizes) ++ "\n"
        biasesStr  = unlines $ show . toList  <$> (nn^.biases)
        weightsStr = unlines $ show . toLists <$> (nn^.weights)
        nnStr      = headerStr ++ biasesStr ++ weightsStr
     in writeFile filepath nnStr

fromFile :: FilePath -> IO Network
fromFile filepath = withFile filepath ReadMode (\handle -> do
    layerSizes' <- (read :: String -> [Int]) <$> hGetLine handle
    biases'     <- forM (tail layerSizes') $ \_ -> do
        line <- hGetLine handle
        return $ fromList $ (read :: String -> [Double]) line
    weights'    <- forM (tail layerSizes') $ \_ -> do
        line <- hGetLine handle
        return $ fromLists $ (read :: String -> [[Double]]) line
    return $ Network { _layerSizes = layerSizes'
                     , _biases     = biases'
                     , _weights    = weights' })
