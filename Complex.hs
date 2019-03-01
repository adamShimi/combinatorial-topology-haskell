{-# LANGUAGE TemplateHaskell #-}

module Complex
    (Complex) where

import Control.Lens
import Data.Set as Set
import Simplex


-- Basic Complexes

newtype Complex a = Complex { __faces :: (SSet (Simplex a)) }
    deriving (Eq,Ord)
makeLenses ''Complex

faces :: (Functor f, Profunctor p) =>
     p (Set (Simplex a0)) (f (Set (Simplex a1)))
     -> p (Complex a0) (f (Complex a1))
faces = _faces . sset

instance (Show a) => Show (Complex a) where
    show = show . (view _faces)


newComplex :: (Ord a) => [Simplex a] -> Complex a
newComplex = Complex . SSet . Set.fromList

standardComplex :: Int -> Complex Int
standardComplex = Complex . SSet . Set.singleton . standardSimplex

dimComplex :: Complex a -> Int
dimComplex =
    (maybe (-1) id) . Set.lookupMax . (Set.map (dimSimplex)) . (view faces)

vertices :: (Ord a) => Complex a -> SSet a
vertices =
    (over sset
          (Set.foldr (\x acc -> Set.union (view simplex x) acc)
                     Set.empty
          )
    )
    . (view _faces)


unionComplex :: (Ord a) => Complex a -> Complex a -> Complex a
unionComplex c1 c2 =
    over faces
         (Set.foldr (\x acc -> if (Set.null (Set.filter (subsimplex x) acc))
                               then Set.insert x acc
                               else acc)
                    Set.empty)
    (over faces (Set.union (view faces c1)) c2)


star :: (Ord a) => Simplex a -> Complex a -> Complex a
star s = over faces (Set.filter ((Set.isSubsetOf (view simplex s)) . (view simplex)))


link :: (Ord a) => Simplex a -> Complex a -> Complex a
link s = over faces (Set.filter (not . (Set.isSubsetOf (view simplex s)) . (view simplex)))


join :: (Ord a) => Complex a -> Complex a -> Complex a
join c1 c2 =
    over faces
         (Set.foldr (\x acc -> Set.union (Set.map (unionSimplex x)
                                                  (view faces c1)
                                         )
                                         acc)
                    Set.empty
         )
         c2


boundary :: (Ord a) => Simplex a -> Complex a
boundary =
    Complex
    . SSet
    . (\s -> Set.foldr (\x acc -> Set.insert (over simplex (Set.delete x) s)
                                             acc
                       )
                       Set.empty $
                       view simplex s)


viewBoundary :: (Ord a) => Simplex a -> Complex (Set a)
viewBoundary = (over faces
                     (Set.map (over simplex
                                    (\y -> Set.map (Set.singleton) y)
                              )
                     )
               ) . boundary


stellarSubdiv :: (Ord a) => Simplex a -> Complex (Set a)
stellarSubdiv simp@(Simplex s) =
    join (newComplex [Simplex $  SSet $ (Set.singleton (view sset s))])
         (viewBoundary simp)
