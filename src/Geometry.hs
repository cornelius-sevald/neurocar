module Geometry ( rotate ) where

import           Linear.V2

rotate :: Floating a => a -> V2 a -> V2 a
rotate ang (V2 x y) = let x' = cos ang * x - sin ang * y
                          y' = sin ang * x + cos ang * y
                       in V2 x' y'
