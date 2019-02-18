module Simplex
    (Simplex,
     newSimplex) where

import Data.Set as Set

newtype Simplex a = Simplex (Set a)

instance (Show a) => Show (Simplex a) where
    show (Simplex set) =
        (++) "{" $ Set.foldr (\new acc -> case acc of
                                         "}" -> (show new) ++ acc
                                         _   -> (show new) ++ ";" ++ acc)
                             "}"
                             set

newSimplex :: (Ord a) => [a] -> Simplex a
newSimplex = Simplex . Set.fromList

standardSimplex :: Int -> Simplex Int
standardSimplex =
    Simplex . Set.fromList . (flip take) (iterate (\x -> x + 1) 0)
