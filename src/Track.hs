{-# LANGUAGE TemplateHaskell #-}
module Track
    ( Track(..)
    , color
    , outerRing
    , innerRing
    , fromCSV
    , carIntersects ) where

import qualified Car                   as C
import           Control.Lens
import qualified Data.ByteString.Lazy  as BL
import           Data.Csv
import           Data.List             (nub)
import qualified Data.Vector           as Vec
import           Data.Word             (Word8)
import           Geometry
import           Geometry.Intersection
import           Linear.V2
import           Linear.V4             (V4)
import           Linear.Vector
import           System.IO

data Track a = Track { _outerRing :: [V2 a]
                     , _innerRing :: [V2 a]
                     , _color     :: V4 Word8 }
                     deriving (Eq, Show)

makeLenses ''Track

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
    let (ir, or) = case decode NoHeader trackData of
                  Left err -> error err
                  Right v -> let toRings = Vec.foldl (\(ir, or) (x0, y0, x1, y1) -> (V2 x0 y0 : ir, V2 x1 y1 : or))
                              in toRings ([], []) v
    return $ Track { _innerRing = nub ir
                   , _outerRing = nub or
                   , _color = color }

carIntersects :: Track Double -> C.Car -> Bool
carIntersects track car =
    let w        = view (C.params . C.width) car / 2
        h        = view (C.params . C.height) car / 2
        c        = view C.position car
        rot      = view C.rotation car
        corners' = [V2 w h, V2 (-w) h, V2 (-w) (-h), V2 w (-h)]
        corners  = map ((c ^+^) . rotate rot) corners'
        cornersC = last corners : corners
        outerC   = last (view outerRing track) : view outerRing track
        innerC   = last (view innerRing track) : view innerRing track
        carSeg   = linkList cornersC
        outerSeg = linkList outerC
        innerSeg = linkList innerC
     in or (intersect <$> carSeg <*> outerSeg) ||
        or (intersect <$> carSeg <*> innerSeg)
