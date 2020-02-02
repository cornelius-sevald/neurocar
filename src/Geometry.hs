module Geometry
    ( Segment
    , Ray
    , rotate
    , segSqLength
    , segLength
    , pointInRect ) where

import           Linear.V2
import           SDL       (Point (..), Rectangle (..))

type Segment a = (V2 a, V2 a)
type Ray a = (V2 a, V2 a)

rotate :: Floating a => a -> V2 a -> V2 a
rotate ang (V2 x y) = let x' = cos ang * x - sin ang * y
                          y' = sin ang * x + cos ang * y
                       in V2 x' y'

segSqLength :: Floating a => Segment a -> a
segSqLength (V2 x0 y0, V2 x1 y1) = (x1-x0)^2 + (y1-y0)^2

segLength :: Floating a => Segment a -> a
segLength = sqrt . segSqLength

pointInRect :: (Ord a, Fractional a) =>
    Rectangle a -> Point V2 a -> Bool
pointInRect (Rectangle (P (V2 cx cy)) (V2 w h)) (P (V2 x y))
  = let cx' = x - cx
        cy' = y - cy
     in 0   <= cx' &&
        cx' <= w   &&
        0   <= cy' &&
        cy' <= h
