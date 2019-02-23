{-# LANGUAGE TemplateHaskell #-}

module Complex
    (Complex) where

import Control.Lens
import Data.Set as Set
import Simplex


-- Basic Complexes

newtype Complex a = Complex { _faces :: (Set (Simplex a)) }
    deriving (Eq,Ord)
makeLenses ''Complex

instance (Show a) => Show (Complex a) where
    show =
        ((++) "{")
        . (Set.foldr (\new acc -> case acc of
                                    "}" -> (show new) ++ acc
                                    _   -> (show new) ++ ";" ++ acc)
                    "}")
        . (view faces)


newComplex :: (Ord a) => [Simplex a] -> Complex a
newComplex = Complex . Set.fromList

standardComplex :: Int -> Complex Int
standardComplex = Complex . Set.singleton . standardSimplex

dimComplex :: Complex a -> Int
dimComplex = (maybe (-1) id) . Set.lookupMax . (Set.map (dimSimplex)) . (view faces)

vertices :: (Ord a) => Complex a -> Set a
vertices =
    (Set.foldr (\x acc -> Set.union (view simplex x) acc) Set.empty) . (view faces)

-- unionComplex :: (Ord a) => Complex a -> Complex a -> Complex a
-- TODO

star :: (Ord a) => Simplex a -> Complex a -> Complex a
star s = over (faces) (Set.filter ((Set.isSubsetOf (view simplex s)) . (view simplex)))


link :: (Ord a) => Simplex a -> Complex a -> Complex a
link s = over (faces) (Set.filter (not . (Set.isSubsetOf (view simplex s)) . (view simplex)))

join :: (Ord a) => Complex a -> Complex a -> Complex a
join c1 c2 =
    over (faces)
         (Set.foldr (\x acc -> Set.union (Set.map (unionSimplex x) (view faces c1))
                                         acc)
                    Set.empty
         )
         c2

-- stellarSubdiv :: (Ord a) => Complex a -> Complex a -> Complex a
-- TODO

