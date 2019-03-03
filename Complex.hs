{-# LANGUAGE TemplateHaskell #-}

module Complex
    (Complex) where

import Control.Lens
import Data.Set as Set
import Simplex


-- Basic Complexes

newtype Complex a = Complex { _faces :: (SSet (Simplex a)) }
    deriving (Eq,Ord)
makeLenses ''Complex

instance (Show a) => Show (Complex a) where
    show = show . (view faces)


newComplex :: (Ord a) => [Simplex a] -> Complex a
newComplex = Complex . newSset

standardComplex :: Int -> Complex Int
standardComplex n = Complex $ newSset [standardSimplex n]

dimComplex :: (Ord a) => Complex a -> Int
dimComplex =
    (maybe (-1) id) . lookupMaxSset . (mapSset (dimSimplex)) . (view faces)

vertices :: (Ord a) => Complex a -> SSet a
vertices =
    (over sset
          (Set.foldr (\x acc -> Set.union (x ^. (simplex . sset)) acc)
                     Set.empty
          )
    )
    . (view faces)


unionComplex :: (Ord a) => Complex a -> Complex a -> Complex a
unionComplex c1 c2 =
    over (faces . sset)
         (Set.foldr (\x acc -> if (Set.null (Set.filter (subsimplex x) acc))
                               then Set.insert x acc
                               else acc)
                    Set.empty)
    (over faces (unionSset (view faces c1)) c2)


clean :: (Ord a) => Complex a -> Complex a
clean = over (faces . sset)
             (Set.foldr (\x acc -> if x == newSimplex []
                                   then acc
                                   else Set.insert x acc)
                        Set.empty
             )

star :: (Ord a) => Simplex a -> Complex a -> Complex a
star s = over faces (filterSset (subsimplex s))


link :: (Ord a) => Simplex a -> Complex a -> Complex a
link s =
    clean .
        (over faces
              (mapSset (over simplex
                             ((flip differenceSset) (s ^. simplex))
                       )
              )
        )

--join :: (Ord a) => Complex a -> Complex a -> Complex a
--join c1 c2 =
--    over faces
--         (Set.foldr (\x acc -> Set.union (Set.map (unionSimplex x)
--                                                  (view faces c1)
--                                         )
--                                         acc)
--                    Set.empty
--         )
--         c2
--
--
--boundary :: (Ord a) => Simplex a -> Complex a
--boundary =
--    Complex
--    . SSet
--    . (\s -> Set.foldr (\x acc -> Set.insert (over simplex (Set.delete x) s)
--                                             acc
--                       )
--                       Set.empty $
--                       view simplex s)
--
--
--viewBoundary :: (Ord a) => Simplex a -> Complex (Set a)
--viewBoundary = (over faces
--                     (Set.map (over simplex
--                                    (\y -> Set.map (Set.singleton) y)
--                              )
--                     )
--               ) . boundary
--
--
--stellarSubdiv :: (Ord a) => Simplex a -> Complex (Set a)
--stellarSubdiv simp@(Simplex s) =
--    join (newComplex [Simplex $  SSet $ (Set.singleton (view sset s))])
--         (viewBoundary simp)
