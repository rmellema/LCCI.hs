{-|
 - This module implements a simple model of Relations between information states.
-}
module LCCI.Relation (
    Relation,
    null,
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
import Prelude hiding (lookup, union, map, null)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import LCCI.Issue
import LCCI.Util(PrettyShow, prettyShow)

-- | The relation type, used for relating information states to other relation
-- states.
newtype Relation a = Relation (Map (State a) (Set (State a)))  deriving (Ord, Eq, Show)

instance (PrettyShow a, Ord a) => PrettyShow (Relation a) where
    prettyShow r = "{" ++ intercalate (Prelude.map showTuple $ minNeed r) ++"}"
        where showTuple (k, v) = "(" ++ prettyShow k ++ ", " ++ prettyShow v ++ ")"
              intercalate = List.intercalate ",\n "
              minNeed r = concatMap makeAlts singeltons
              makeAlts k = [(k, i') | i' <- alternatives $ lookup r k]
              singeltons = filter (\s -> Set.size s == 1) $ keys r

-- | Check if a given relation is empty.
null :: Relation a -> Bool
null (Relation r) = Map.null r

-- | The empty relation, i.e., the relation that maps nothing to something else.
empty :: Relation a
empty = Relation Map.empty

-- | Add a relation between two information states.
insert :: (Ord a) => Relation a -> State a -> State a -> Relation a
insert (Relation r) s t = Relation (Map.insertWith Set.union s t' r)
    where t' = Set.singleton t

-- | Read a relation from a given state map
fromStateMap :: (World a) => StateMap a -> Relation a
fromStateMap = Relation . Map.foldrWithKey f Map.empty
    where f k = Map.insert (state [k])

-- | Read a relation from a list of tuples.
fromList :: (Ord a) => [(State a, State a)] -> Relation a
fromList xs = makeValid $ makeKeys $ Prelude.foldr f empty xs
    where f (s, t) r = insert r s t

-- | Turn this relation into a list of tuples
toList :: Relation a -> [(State a, State a)]
toList (Relation r) = Map.foldrWithKey f [] r
    where f k v a = [(k, v') | v' <- Set.elems v] ++ a

-- | Take the union of two relations.
union :: (Ord a) => Relation a -> Relation a -> Relation a
union (Relation r1) (Relation r2) = Relation $ Map.unionWith Set.union r1 r2

-- | Get all the information states that this relation relates to others.
keys :: Relation a -> [State a]
keys (Relation r) = Map.keys r

-- | Compose two relations together
compose :: (Ord a) => Relation a -> Relation a  -> Relation a
compose (Relation r1) r2 = Relation $ Map.foldrWithKey f Map.empty r1
    where f k i r = Set.foldr (Map.insertWith Set.union k) r $ Set.map (lookup r2) i

-- | Take the reflexive closure of this relation.
reflexiveClosure :: (Ord a) => Relation a -> Relation a
reflexiveClosure (Relation r) = Relation $ Map.mapWithKey f r
    where f k = Set.union (issue [k])

-- | Take the transitiveClosure of this relation.
transitiveClosure :: (Ord a) => Relation a -> Relation a
transitiveClosure (Relation r) = Relation $ Map.foldrWithKey (\k ks r' ->
    Map.unionWith Set.union r' $ Map.foldrWithKey (\s rs r'' ->
        Map.insertWith Set.union s (
            if k `Set.member` rs then
                issue $ Map.keys $ Map.filterWithKey (\t _ -> t `Set.member` ks) r''
            else Set.empty) r'') r' r') r r

-- | Map the given function @f@ over the domain of the relation.
mapDomain :: (Ord b) => (State a -> b) -> Relation a -> [b]
mapDomain f (Relation r) = Map.elems $ Map.mapWithKey (\k _ -> f k) r

-- | Lookup to which states this relation relates a given information state.
-- Returns an empty set if it maps it to none.
lookup :: (Ord a) => Relation a -> State a -> Set (State a)
lookup (Relation r) s = unpack $ Map.lookup s r
    where unpack (Just x) = x
          unpack Nothing  = Set.empty

-- | Check if the empty state is related to nothing.
checkEmptySet :: (Ord a) => Relation a -> Bool
checkEmptySet r = Prelude.null $ lookup r (state [])

-- | Check to see if every state is related to the empty state
checkEscape :: Relation a -> Bool
checkEscape (Relation r) = not $ any Set.null r

-- | Check if all the elements in the relation are downward closed.
checkRelationLeft :: (Ord a) => Relation a -> Bool
checkRelationLeft (Relation r) = all isDownwardClosed r

-- | Check to see that if a state `s` is related to a state `t`, then there is
-- a world `w` in `s` such that `w` is related to `t`.
checkRelationRight :: (Ord a) => Relation a -> Bool
checkRelationRight (Relation r) = and $ Map.mapWithKey check r
    where check k v
            | Set.size k == 1 = True
            | otherwise = and $ Set.map (\s -> or $ Set.map (\w ->
                    s `Set.member` lookup (Relation r) (state [w])) k) v

checkRelation :: (Ord a) => Relation a -> Bool
checkRelation r = checkRelationLeft r && checkRelationRight r

-- | Check if the given relation adheres to all the properties that an IE-PDL
-- relation should.
isValid :: (Ord a) => Relation a -> Bool
isValid r = checkEmptySet r && checkEscape r && checkRelation r

-- | Make all the keys for a given relation.
makeKeys :: (Ord a) => Relation a -> Relation a
makeKeys (Relation r) = Relation (Map.union r $ Set.foldr f Map.empty keySet)
    where f s = Map.insert s $ Set.singleton Set.empty
          keySet = Set.filter (not . Set.null) $ powerset $ Set.unions $ Map.keys r

-- | Make sure that the given relation adheres to all the properties it has to.
makeValid :: (Ord a) => Relation a -> Relation a
makeValid (Relation r) = Relation $ Map.mapWithKey (\s ts ->
    downwardClose $ Set.unions (ts : Set.elems (Set.map (\w ->
                                     lookup (Relation r) (state [w])) s))) r
