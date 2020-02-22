module Simplex
    (Proc(Proc),
    allProcs,
    procs,
    Simplex(Simplex),
    simplex,
    newSimplex,
    standardSimplex,
    dimSimplex,
    unionSimplex,
    subSimplex,
    standardColSimplex,
    coloring,
    collapse) where

import Data.Set as Set

-- Processes

data Proc = Proc {num :: Int}
    deriving (Ord, Eq)

instance Show Proc where
    show = ((++) "p") . show . num

allProcs :: [Proc]
allProcs = [Proc n | n <- [1..]]

procs :: Int -> [Proc]
procs = (flip Prelude.take) allProcs


-- Basic Simplexes

newtype Simplex a = Simplex { simplex :: Set a }
    deriving (Eq,Ord)

instance (Show a) => Show (Simplex a) where
    show = show . simplex

newSimplex :: (Ord a) => [a] -> Simplex a
newSimplex = Simplex . Set.fromList

standardSimplex :: Int -> Simplex Int
standardSimplex = Simplex . Set.fromList . (flip Prelude.take) [1..]

dimSimplex :: (Ord a) => Simplex a -> Int
dimSimplex = (+ (-1)) . size . simplex

unionSimplex :: (Ord a) => Simplex a -> Simplex a -> Simplex a
unionSimplex s1 s2 = Simplex $ Set.union (simplex s1) (simplex s2)

subSimplex :: (Ord a) => Simplex a -> Simplex a -> Bool
subSimplex s1 s2 = Set.isSubsetOf (simplex s1) (simplex s2)


-- Colored Simplexes


standardColSimplex :: Int -> Simplex (Proc,Int)
standardColSimplex n = (Simplex . Set.fromList) $ zip (procs n) [1..n]

coloring :: (Ord a, Ord c) => (a -> c) -> Simplex a -> Simplex (c,a)
coloring f = Simplex . (Set.map (\x -> (f x,x))) . simplex

collapse :: (Ord a, Ord c) => Simplex (c,a) -> Simplex a
collapse = Simplex . (Set.map snd) . simplex

