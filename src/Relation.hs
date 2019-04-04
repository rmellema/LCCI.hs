{-|
 - This module implements a simple model of Relations between information states.
-}
module Relation (
    Relation,
    Relation.null,
    empty,
    insert,
    fromList,
    keys,
    toList,
    union,
    compose,
    reflexiveClosure,
    transitiveClosure,
    mapDomain,
    lookup,
    isValid,
    makeKeys,
    makeValid,
) where
import qualified Prelude
import Prelude hiding (lookup, union, map)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import Issue
import Util(PrettyShow, prettyShow)

newtype Relation a = Relation (Map (State a) (Set (State a)))  deriving (Ord, Eq, Show)

instance (PrettyShow a, Ord a) => PrettyShow (Relation a) where
    prettyShow r = "{" ++ intercalate (Prelude.map showTuple $ minNeed r) ++"}"
        where showTuple (k, v) = "(" ++ prettyShow k ++ ", " ++ prettyShow v ++ ")"
              intercalate = List.intercalate ",\n "
              minNeed r = concatMap makeAlts singeltons
              makeAlts k = [(k, i') | i' <- alternatives $ lookup r k]
              singeltons = filter (\s -> Set.size s == 1) $ keys r

null :: Relation a -> Bool
null (Relation r) = Map.null r

empty :: Relation a
empty = Relation Map.empty

insert :: (Ord a) => Relation a -> State a -> State a -> Relation a
insert (Relation r) s t = Relation (Map.insertWith Set.union s t' r)
    where t' = Set.singleton t

fromList :: (Ord a) => [(State a, State a)] -> Relation a
fromList xs = makeValid $ makeKeys $ Prelude.foldr f empty xs
    where f (s, t) r = insert r s t

toList :: Relation a -> [(State a, State a)]
toList (Relation r) = Map.foldrWithKey f [] r
    where f k v a = [(k, v') | v' <- Set.elems v] ++ a

union :: (Ord a) => Relation a -> Relation a -> Relation a
union (Relation r1) (Relation r2) = Relation $ Map.unionWith Set.union r1 r2

keys :: Relation a -> [State a]
keys (Relation r) = Map.keys r

-- | Compose two relations together
compose :: (Ord a) => Relation a -> Relation a  -> Relation a
compose (Relation r1) r2 = Map.foldrWithKey f empty r1
    where f k v r = Set.foldr (\s r' -> insert r' k s) r v

reflexiveClosure :: (Ord a) => Relation a -> Relation a
reflexiveClosure (Relation r) = Relation $ Map.mapWithKey f r
    where f k = Set.union (issue [k])

transitiveClosure :: (Ord a) => Relation a -> Relation a
transitiveClosure (Relation r) = Relation $ Map.foldrWithKey (\k ks r' ->
    Map.unionWith Set.union r' $ Map.foldrWithKey (\s rs r'' ->
        Map.insertWith Set.union s (
            if k `Set.member` rs then
                issue $ Map.keys $ Map.filterWithKey (\t _ -> t `Set.member` ks) r''
            else Set.empty) r'') r' r') r r

mapDomain :: (Ord b) => (State a -> b) -> Relation a -> [b]
mapDomain f (Relation r) = Map.elems $ Map.mapWithKey (\k _ -> f k) r

lookup :: (Ord a) => Relation a -> State a -> Set (State a)
lookup (Relation r) s = unpack $ Map.lookup s r
    where unpack (Just x) = x
          unpack Nothing  = Set.empty

checkEmptySet :: (Ord a) => Relation a -> Bool
checkEmptySet r = Prelude.null $ lookup r (state [])

checkEscape :: Relation a -> Bool
checkEscape (Relation r) = not $ any Set.null r

checkRelationLeft :: (Ord a) => Relation a -> Bool
checkRelationLeft (Relation r) = all isDownwardClosed r

checkRelationRight :: (Ord a) => Relation a -> Bool
checkRelationRight (Relation r) = and $ Map.mapWithKey check r
    where check k v = and $ Map.mapWithKey (\k' v' ->
                        not (Set.isSubsetOf k k') || Set.isSubsetOf v v') r

checkRelation :: (Ord a) => Relation a -> Bool
checkRelation r = checkRelationLeft r && checkRelationRight r

isValid :: (Ord a) => Relation a -> Bool
isValid r = checkEmptySet r && checkEscape r && checkRelation r

makeKeys :: (Ord a) => Relation a -> Relation a
makeKeys (Relation r) = Relation (Map.union r $ Set.foldr f Map.empty keySet)
    where f s = Map.insert s $ Set.singleton Set.empty
          keySet = Set.filter (not . Set.null) $ powerset $ Set.unions $ Map.keys r

makeValid :: (Ord a) => Relation a -> Relation a
makeValid (Relation r) = Relation $ Map.mapWithKey (\ s ts ->
    downwardClose $ Set.unions $ ts : Map.elems (Map.filterWithKey (\ t us ->
        t `Set.isSubsetOf` s) r)) r
