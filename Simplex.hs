{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Simplex
    (SSet(SSet),
    sset,
    Simplex(Simplex),
    simplex,
    newSimplex,
    standardSimplex,
    dimSimplex,
    unionSimplex,
    subsimplex,
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


-- My own sets, that behave nicely.

newtype SSet a = SSet { _sset :: (Set a)}
    deriving (Eq)
makeLenses ''SSet

instance (Show a) => Show (SSet a) where
    show =
        ((++) "{")
        . (Set.foldr (\new acc -> case acc of
                                    "}" -> (show new) ++ acc
                                    _   -> (show new) ++ ";" ++ acc)
                    "}")
        . (view sset)


-- If comparable, then inclusion, if not, lexicographic.
instance (Ord a) => Ord (SSet a) where
    (<=) (SSet s1) (SSet s2) =
        if Set.isSubsetOf s1 s2
        then True
        else if Set.isSubsetOf s2 s1
             then False
             else (<=) s1 s2


subset :: (Ord a) => SSet a -> SSet a -> Bool
subset (SSet s1) (SSet s2) = Set.isSubsetOf s1 s2

newSset :: (Ord a) => [a] -> SSet a
newSset = SSet . Set.fromList

unionSset :: (Ord a) => SSet a -> SSet a -> SSet a
unionSset (SSet s1) (SSet s2) = SSet (Set.union s1 s2)

mapSset :: (Ord a,Ord b) => (a -> b) -> SSet a -> SSet b
mapSset f = over sset (Set.map f)


-- Basic Simplexes

newtype Simplex a = Simplex { __simplex :: (SSet a) }
    deriving (Eq,Ord)
makeLenses ''Simplex


simplex :: (Functor f, Profunctor p) =>
    p (Set a0) (f (Set a1)) -> p (Simplex a0) (f (Simplex a1))
simplex = _simplex . sset

instance (Show a) => Show (Simplex a) where
    show = show . (view _simplex)

newSimplex :: (Ord a) => [a] -> Simplex a
newSimplex = Simplex . newSset

standardSimplex :: Int -> Simplex Int
standardSimplex =
    Simplex . newSset . (flip Prelude.take) [1..]

dimSimplex :: Simplex a -> Int
dimSimplex = ((+) (-1)) . size . (view (simplex))

unionSimplex :: (Ord a) => Simplex a -> Simplex a -> Simplex a
unionSimplex s1 = over _simplex (unionSset (s1 ^. _simplex))

subsimplex :: (Ord a) => Simplex a -> Simplex a -> Bool
subsimplex s1 = (subset (s1 ^. _simplex)) . (view _simplex)


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
    ColSimplex . Simplex . SSet . Set.fromList $ zip (procs n) (Prelude.take n [1..])

dimColSimplex :: ColSimplex c a -> Int
dimColSimplex = dimSimplex . (view colSimplex)

coloring :: (Ord a, Ord c) => (a -> c) -> Simplex a -> ColSimplex c a
coloring f = ColSimplex . (over simplex (Set.map (\x -> (f x,x))))

collapse :: (Ord a, Ord c) => ColSimplex c a -> Simplex a
collapse = (over simplex (Set.map (snd))) . (view colSimplex)
