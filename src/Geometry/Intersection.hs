module Geometry.Intersection
    ( Segment
    , intersect ) where

import           Linear.V2
import           Linear.Vector

type Segment a = (V2 a, V2 a)

-- https://algorithmtutor.com/Computational-Geometry/Check-if-two-line-segment-intersect/

direction :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> a
direction v0 v1 v2 = crossZ (v2 ^-^ v0) (v1 ^-^ v0)

onSegment :: (Num a, Ord a) => Segment a -> V2 a -> Bool
onSegment (V2 v0x v0y, V2 v1x v1y) (V2 px py) =
    (min v0x v1x <= px) &&
    (max v0x v1x >= px) &&
    (min v0y v1y <= py) &&
    (max v0y v1y >= py)

intersect :: (Num a, Ord a) => Segment a -> Segment a -> Bool
intersect (v0, v1) (v2, v3) =
    let d0 = direction v2 v3 v0
        d1 = direction v2 v3 v1
        d2 = direction v0 v1 v2
        d3 = direction v0 v1 v3
     in (((d0 > 0 && d1 < 0) || (d0 < 0 && d1 > 0)) &&
        ((d2 > 0 && d3 < 0) || (d2 < 0 && d3 > 0))) ||
        d0 == 0 && onSegment (v2, v3) v0            ||
        d1 == 0 && onSegment (v2, v3) v1            ||
        d2 == 0 && onSegment (v0, v1) v2            ||
        d3 == 0 && onSegment (v0, v1) v3
