module Simplex
    (Simplex,
     newSimplex) where

import Data.Set as Set



-- Basic Simplexes

newtype Simplex a = Simplex (Set a)
    deriving (Eq)

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
    Simplex . Set.fromList . (flip take) [1..]



-- Processes

data Proc = Proc Int
    deriving (Ord, Eq)

instance Show Proc where
    show (Proc n) = "p" ++ (show n)

allProcs :: [Proc]
allProcs = [Proc n | n <- [1..]]

procs :: Int -> [Proc]
procs = (flip take) allProcs



-- Colored Simplexes

newtype ColSimplex c a  = ColSimplex (Simplex (c,a))

instance (Show c, Show a) => Show (ColSimplex c a) where
    show (ColSimplex s) = show s

newColSimplex :: (Ord c, Ord a) => [(c,a)] -> ColSimplex c a
newColSimplex = ColSimplex . newSimplex

standardColSimplex :: Int -> ColSimplex Proc Int
standardColSimplex n =
    ColSimplex . Simplex . Set.fromList $ zip (procs n) (take n [1..])

coloring :: (Ord a, Ord c) => Simplex a -> (a -> c) -> ColSimplex c a
coloring (Simplex s) f = ColSimplex . Simplex $ Set.map (\x -> (f x,x)) s

collapse :: (Ord a, Ord c) => ColSimplex c a -> Simplex a
collapse (ColSimplex (Simplex s)) = Simplex $ Set.map (snd) s
