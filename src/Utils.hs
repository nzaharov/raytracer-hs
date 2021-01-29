module Utils where

import Control.Monad

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l
  | n > 0 = take n l : chunks n (drop n l)
  | otherwise = error "Negative or zero n"

maybeUnless :: Bool -> Maybe a -> Maybe a
maybeUnless p s = join $ if p then Nothing else Just s