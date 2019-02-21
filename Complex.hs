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

starComplex :: (Ord a) => Simplex a -> Complex a -> Complex a
starComplex s = over (faces) (Set.filter ((Set.isSubsetOf (view simplex s)) . (view simplex)))
