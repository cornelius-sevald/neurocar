module Geometry.Intersection
    ( segSegIntersection
    , raySegIntersection
    , segRayIntersection
    , segSegDoesIntersect
    , raySegDoesIntersect
    , segRayDoesIntersect ) where

import           Data.Maybe
import           Geometry
import           Linear.V2
import           Linear.Vector

(×) :: Num a => V2 a -> V2 a -> a
(×) = crossZ

-- https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect/

lineIntersectParameters :: (Eq a, Fractional a) =>
    (V2 a, V2 a) -> (V2 a, V2 a) -> Maybe (a, a)
lineIntersectParameters (p, r) (q, s)
  | rs == 0   = Nothing
  | otherwise = Just (t, u)
  where qp =  q ^-^ p
        rs =  r  ×  s
        t  = qp  ×  s / rs
        u  = qp  ×  r / rs

segSegIntersection :: (Eq a, Ord a, Fractional a) =>
    Segment a -> Segment a -> Maybe (V2 a)
segSegIntersection seg0 seg1 =
    let p = fst seg0
        q = fst seg1
        r = snd seg0 ^-^ p
        s = snd seg1 ^-^ q
     in case lineIntersectParameters (p, r) (q, s) of
          Nothing     -> Nothing
          Just (t, u) -> if 0 <= t && t <= 1 && 0 <= u && u <= 1
                            then Just (p ^+^ r ^* t)
                            else Nothing

raySegIntersection :: (Eq a, Ord a, Fractional a) =>
    Ray a -> Segment a -> Maybe (V2 a)
raySegIntersection ray seg =
    let p = fst ray
        q = fst seg
        r = snd ray
        s = snd seg ^-^ q
     in case lineIntersectParameters (p, r) (q, s) of
          Nothing     -> Nothing
          Just (t, u) -> if 0 <= t && 0 <= u && u <= 1
                            then Just (p ^+^ r ^* t)
                            else Nothing

segRayIntersection :: (Eq a, Ord a, Fractional a) =>
    Segment a -> Ray a -> Maybe (V2 a)
segRayIntersection = flip raySegIntersection

segSegDoesIntersect :: (Eq a, Ord a, Fractional a) =>
    Segment a -> Segment a -> Bool
segSegDoesIntersect seg0 seg1 = isJust $ segSegIntersection seg0 seg1

raySegDoesIntersect :: (Eq a, Ord a, Fractional a) =>
    Ray a -> Segment a -> Bool
raySegDoesIntersect ray seg = isJust $ raySegIntersection ray seg

segRayDoesIntersect :: (Eq a, Ord a, Fractional a) =>
    Segment a -> Ray a -> Bool
segRayDoesIntersect = flip raySegDoesIntersect
