module Util where

-- Link a list into a list of pairs, sharing one common element in each pair
linkList :: [a] -> [(a, a)]
linkList []         = []
linkList [a]        = error "linkList - singleton list"
linkList [x0, x1]   = [(x0, x1)]
linkList (x0:x1:xs) = (x0, x1) : linkList (x1:xs)

