module Scene where

import Data.Foldable (Foldable (foldl'))
import Material

newtype Scene a = Scene [a] deriving (Show)

instance Intersectable a => Intersectable (Scene a) where
  intersect (Scene objects) ray min max = fst $ foldl' f (Nothing, max) objects
    where
      f acc obj = case intersect obj ray min max of
        Nothing -> acc
        Just hit -> if t hit < snd acc then (Just hit, t hit) else acc
