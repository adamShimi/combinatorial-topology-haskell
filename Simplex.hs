{-# LANGUAGE TemplateHaskell #-}

module Simplex
    (Simplex,
     newSimplex) where

import Control.Lens
import Data.Set as Set



-- Basic Simplexes

newtype Simplex a = Simplex { _simplex :: (Set a) }
    deriving (Eq)
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
    Simplex . Set.fromList . (flip take) [1..]

dimensionSimplex :: Simplex a -> Int
dimensionSimplex = size . (view simplex)



-- Processes

data Proc = Proc {num :: Int}
    deriving (Ord, Eq)

instance Show Proc where
    show = ((++) "p") . show . num

allProcs :: [Proc]
allProcs = [Proc n | n <- [1..]]

procs :: Int -> [Proc]
procs = (flip take) allProcs



-- Colored Simplexes

newtype ColSimplex c a  = ColSimplex { _colSimplex :: (Simplex (c,a)) }
makeLenses ''ColSimplex

instance (Show c, Show a) => Show (ColSimplex c a) where
    show = show . (view colSimplex)

newColSimplex :: (Ord c, Ord a) => [(c,a)] -> ColSimplex c a
newColSimplex = ColSimplex . newSimplex

standardColSimplex :: Int -> ColSimplex Proc Int
standardColSimplex n =
    ColSimplex . Simplex . Set.fromList $ zip (procs n) (take n [1..])

dimensionColSimplex :: ColSimplex c a -> Int
dimensionColSimplex = dimensionSimplex . (view colSimplex)

coloring :: (Ord a, Ord c) => (a -> c) -> Simplex a -> ColSimplex c a
coloring f = ColSimplex . (over simplex (Set.map (\x -> (f x,x))))

collapse :: (Ord a, Ord c) => ColSimplex c a -> Simplex a
collapse = (over simplex (Set.map (snd))) . (view colSimplex)
