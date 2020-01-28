{-# LANGUAGE LambdaCase #-}
module AI.NeuroCar where

import           AI.GeneticAlgorithm   as GA
import qualified AI.NeuralNetwork      as NN
import qualified Car                   as C
import           Control.Lens
import           Control.Monad.Loops
import           Control.Monad.State
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Word             (Word32)
import           Game
import qualified Geometry              as G
import qualified Numeric.LinearAlgebra as LA
import           System.Random
import qualified Track                 as T
import           World

rayAngle :: Double
rayAngle = pi

rayCount :: Int
rayCount = 7

evolveCar :: Int -> Int -> Int -> Double -> Double -> Double -> Word32 ->
    C.CarParams -> T.Track Double -> [Population Double]
evolveCar seed generations popSize mutChance mutStrength time deltaTicks carParams track = do
    let gen = mkStdGen seed
    let world = initWorld carParams track time
    let evolveLoop nn = Identity . gameLoop' (getNetworkInput nn) deltaTicks
    let runGame nn = runIdentity $ iterateUntilM (\w -> w^.gameState /= GameRunning) (evolveLoop nn) world
    let fitfunc nn = fromIntegral $ view score (runGame nn)
    let mutfunc = GA.mutate mutChance mutStrength
    let indGen = NN.newNetwork [3+rayCount, 15, 15, 2]
    let evoFunc = evolves generations popSize indGen fitfunc mutfunc
    evalState evoFunc gen


getNetworkInput :: NN.Network -> World -> [Input]
getNetworkInput nn w =
    let push a        = (state $ \xs -> ((),a:xs)) :: State [Input] ()
        rays          = C.shootRays rayAngle rayCount $ w^.car
        origin        = w^.car.C.position
        intersections = T.rayIntersection (w^.track) <$> rays
        distances     = (\case Nothing -> 0
                               Just v  -> G.segLength (origin, v)) <$> intersections
        velocity      = C.localVelocity $ w^.car
        rotation      = w^.car.C.rotation
        x             = rotation : toList velocity ++ distances
        y             = LA.toList $ NN.feedforward (LA.fromList x) nn
     in execState (do { when (head y < 1/3) (push GoBackward)
                      ; when (head y > 2/3) (push GoForward)
                      ; when (y !! 1 < 1/3) (push GoLeft)
                      ; when (y !! 1 > 2/3) (push GoRight) }) []

