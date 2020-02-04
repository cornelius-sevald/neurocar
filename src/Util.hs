module Util where

import           Data.Maybe
import           SDL

-- Link a list into a list of pairs, sharing one common element in each pair
linkList :: [a] -> [(a, a)]
linkList []         = []
linkList [a]        = error "linkList - singleton list"
linkList [x0, x1]   = [(x0, x1)]
linkList (x0:x1:xs) = (x0, x1) : linkList (x1:xs)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

maybeToMonoid :: Monoid m => Maybe m -> m
maybeToMonoid = fromMaybe mempty

eventIsButtonPress :: Keycode -> Event -> Bool
eventIsButtonPress code event =
            case eventPayload event of
              KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == code
              _ -> False
