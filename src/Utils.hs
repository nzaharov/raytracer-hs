module Utils where

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l
  | n > 0 = take n l : chunks n (drop n l)
  | otherwise = error "Negative or zero n"