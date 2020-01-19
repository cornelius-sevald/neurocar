{-# LANGUAGE TemplateHaskell #-}
module Track
    ( Track(..)
    , color
    , checkpointIndex
    , rings
    , getCheckpoint
    , fromCSV
    , carIntersectsCheckpoint
    , carIntersects ) where

import qualified Car                   as C
import           Control.Lens
import           Control.Monad.State
import qualified Data.ByteString.Lazy  as BL
import           Data.Csv
import qualified Data.Vector           as Vec
import           Data.Word             (Word8)
import           Geometry
import           Geometry.Intersection
import           Linear.V2
import           Linear.V4             (V4)
import           Linear.Vector
import           System.IO

-- An inner and outer ring
type Rings a = [(V2 a, V2 a)]

data Track a = Track { _rings           :: Rings a
                     , _checkpointIndex :: Int
                     , _color           :: V4 Word8 }
                     deriving (Eq, Show)

makeLenses ''Track

getCheckpoint :: Track a -> (V2 a, V2 a)
getCheckpoint track = cycle (view rings track) !! view checkpointIndex track

-- Utility function
-- Link a list into a list of pairs, sharing one common element in each pair
linkList :: [a] -> [(a, a)]
linkList []         = []
linkList [a]        = error "linkList - singleton list"
linkList [x0, x1]   = [(x0, x1)]
linkList (x0:x1:xs) = (x0, x1) : linkList (x1:xs)

fromCSV :: V4 Word8 -> FilePath -> IO (Track Double)
fromCSV color filepath = do
    trackData <- BL.readFile filepath
    let rngs = case decode NoHeader trackData of
                  Left err -> error err
                  Right v -> let toRings = Vec.foldr (\(x0, y0, x1, y1) rngs -> (V2 x0 y0, V2 x1 y1) : rngs)
                              in toRings [] v
    return $ Track { _rings = rngs
                   , _checkpointIndex = 0
                   , _color = color }

carIntersects :: Track Double -> C.Car -> Bool
carIntersects track car =
    let corners  = C.globalCorners car
        cornersC = last corners : corners
        ringC    = last (view rings track) : view rings track
        carSeg   = linkList cornersC
        innerSeg = linkList $ map fst ringC
        outerSeg = linkList $ map snd ringC
     in or (intersect <$> carSeg <*> outerSeg) ||
        or (intersect <$> carSeg <*> innerSeg)

carIntersectsCheckpoint :: Track Double -> C.Car -> Bool
carIntersectsCheckpoint track car =
    let corners    = C.globalCorners car
        cornersC   = last corners : corners
        carSeg     = linkList cornersC
        checkpoint = getCheckpoint track
     in or (intersect checkpoint <$> carSeg)
