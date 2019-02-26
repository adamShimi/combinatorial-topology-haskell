{-# LANGUAGE TemplateHaskell #-}

module Simplex
    (Simplex(Simplex),
    simplex,
    newSimplex,
    standardSimplex,
    dimSimplex,
    unionSimplex,
    isSubsimplexOf,
    Proc(Proc),
    allProcs,
    procs,
    ColSimplex(ColSimplex),
    colSimplex,
    newColSimplex,
    standardColSimplex,
    dimColSimplex,
    coloring,
    collapse) where

import Control.Lens
import Data.Set as Set


-- Redéfinir les Sets avec de vrais instances de Show et Ord, parce
-- que ce n'est pas possible là.


-- Basic Simplexes

newtype Simplex a = Simplex { _simplex :: (Set a) }
    deriving (Eq,Ord)
makeLenses ''Simplex

instance (Show a) => Show (Simplex a) where
    show =
        ((++) "{")
        . (Set.foldr (\new acc -> case acc of
                                    "}" -> (show new) ++ acc
                                    _   -> (show new) ++ ";" ++ acc)
                    "}")
        . (view simplex)

newSimplex :: (Ord a) => [a] -> Simplex a
newSimplex = Simplex . Set.fromList

standardSimplex :: Int -> Simplex Int
standardSimplex =
    Simplex . Set.fromList . (flip Prelude.take) [1..]

dimSimplex :: Simplex a -> Int
dimSimplex = ((+) (-1)) . size . (view simplex)

unionSimplex :: (Ord a) => Simplex a -> Simplex a -> Simplex a
unionSimplex s1 = over (simplex) (Set.union (view (simplex) s1))

isSubsimplexOf :: (Ord a) => Simplex a -> Simplex a -> Bool
isSubsimplexOf s1 = (Set.isSubsetOf (view simplex s1)) . (view simplex)


-- Processes

data Proc = Proc {num :: Int}
    deriving (Ord, Eq)

instance Show Proc where
    show = ((++) "p") . show . num

allProcs :: [Proc]
allProcs = [Proc n | n <- [1..]]

procs :: Int -> [Proc]
procs = (flip Prelude.take) allProcs



-- Colored Simplexes

newtype ColSimplex c a  = ColSimplex { _colSimplex :: (Simplex (c,a)) }
makeLenses ''ColSimplex

instance (Show c, Show a) => Show (ColSimplex c a) where
    show = show . (view colSimplex)

newColSimplex :: (Ord c, Ord a) => [(c,a)] -> ColSimplex c a
newColSimplex = ColSimplex . newSimplex

standardColSimplex :: Int -> ColSimplex Proc Int
standardColSimplex n =
    ColSimplex . Simplex . Set.fromList $ zip (procs n) (Prelude.take n [1..])

dimColSimplex :: ColSimplex c a -> Int
dimColSimplex = dimSimplex . (view colSimplex)

coloring :: (Ord a, Ord c) => (a -> c) -> Simplex a -> ColSimplex c a
coloring f = ColSimplex . (over simplex (Set.map (\x -> (f x,x))))

collapse :: (Ord a, Ord c) => ColSimplex c a -> Simplex a
collapse = (over simplex (Set.map (snd))) . (view colSimplex)
