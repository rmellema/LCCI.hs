{-|
 - This module implements a simple model of Relations between information states.
-}
module Relation (
    Relation,
    Relation.null,
    empty,
    insert,
    fromList,
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

newtype Relation = Relation (Map State (Set State))  deriving (Ord, Eq)

instance Show Relation where
    show r = "{" ++ intercalate (Prelude.map showTuple $ toList r)  ++ "}"
        where showTuple (k, v) = "(" ++ showState k ++ ", " ++ showState v ++ ")"
              intercalate = List.intercalate ",\n "

null :: Relation -> Bool
null (Relation r) = Map.null r

empty :: Relation
empty = Relation Map.empty

insert :: Relation -> State -> State -> Relation
insert (Relation r) s t = Relation (Map.insertWith Set.union s t' r)
    where t' = Set.singleton t

fromList :: [(State, State)] -> Relation
fromList xs = makeValid $ makeKeys $ Prelude.foldr f empty xs
    where f (s, t) r = insert r s t

toList :: Relation -> [(State, State)]
toList (Relation r) = Map.foldrWithKey f [] r
    where f k v a = [(k, v') | v' <- Set.elems v] ++ a

union :: Relation -> Relation -> Relation
union (Relation r1) (Relation r2) = Relation $ Map.unionWith Set.union r1 r2

compose :: Relation -> Relation -> Relation
compose (Relation r1) r2 = Map.foldrWithKey f empty r1
    where f k v r = Set.foldr (\s r' -> insert r' k s) r v

reflexiveClosure :: Relation -> Relation
reflexiveClosure (Relation r) = Relation $ Map.mapWithKey f r
    where f k = Set.union (issue [k])

transitiveClosure :: Relation -> Relation
transitiveClosure (Relation r) = Relation $ Map.foldrWithKey (\k ks r' ->
    Map.unionWith Set.union r' $ Map.foldrWithKey (\s rs r'' ->
        Map.insertWith Set.union s (
            if k `Set.member` rs then
                issue $ Map.keys $ Map.filterWithKey (\t _ -> t `Set.member` ks) r''
            else Set.empty) r'') r' r') r r

mapDomain :: (Ord b) => (State -> b) -> Relation -> [b]
mapDomain f (Relation r) = Map.elems $ Map.mapWithKey (\k _ -> f k) r

lookup :: Relation -> State -> Set State
lookup (Relation r) s = unpack $ Map.lookup s r
    where unpack (Just x) = x
          unpack Nothing  = Set.empty

checkEmptySet :: Relation -> Bool
checkEmptySet r = Prelude.null $ lookup r (state [])

checkEscape :: Relation -> Bool
checkEscape (Relation r) = not $ any Set.null r

checkRelationLeft :: Relation -> Bool
checkRelationLeft (Relation r) = all isDownwardClosed r

checkRelationRight :: Relation -> Bool
checkRelationRight (Relation r) = and $ Map.mapWithKey check r
    where check k v = and $ Map.mapWithKey (\k' v' ->
                        not (Set.isSubsetOf k k') || Set.isSubsetOf v v') r

checkRelation :: Relation -> Bool
checkRelation r = checkRelationLeft r && checkRelationRight r

isValid :: Relation -> Bool
isValid r = checkEmptySet r && checkEscape r && checkRelation r

makeKeys :: Relation -> Relation
makeKeys (Relation r) = Relation (Map.union r $ Set.foldr f Map.empty keySet)
    where f s = Map.insert s $ Set.singleton Set.empty
          keySet = Set.filter (not . Set.null) $ powerset $ Set.unions $ Map.keys r

makeValid :: Relation -> Relation
makeValid (Relation r) = Relation $ Map.mapWithKey (\ s ts ->
    downwardClose $ Set.unions $ ts : Map.elems (Map.filterWithKey (\ t us ->
        t `Set.isSubsetOf` s) r)) r
