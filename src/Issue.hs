{-|
This module implements the structures from Inquisitive Semantics
Information States, Issues, and State Maps.
-}
module Issue (
    World(..),
    State,
    state,
    Issue,
    showIssue,
    emptyIssue,
    issue,
    powerset,
    downwardClose,
    isDownwardClosed,
    alternatives,
    StateMap,
    showStateMap,
    showStateMaps,
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List(intercalate, subsequences)
import Syntax
import Util(showSet)

-- | A world in an LCCI model
newtype World a = World a deriving (Ord, Eq)

instance (Show a) => Show (World a) where
    show (World w) = 'w' : show w

-- | An information state
type State a = Set.Set (World a)

-- | A convience function for defining States. Creates a state with every
-- world in the input list
state :: (Ord a) => [World a] -> State a
state = Set.fromList

-- | A new issue. This will not garantee that an Issue is downward closed.
type Issue a = Set.Set (Set.Set a)

-- | A prettier way to show Issues
showIssue :: (Ord a, Show a) => Issue a -> String
showIssue s = '{' : intercalate ", " (map showSet s') ++ "}v"
    where s' = alternatives s

-- | Create the powerset of a given set.
powerset :: (Ord a) => Set.Set a -> Set.Set (Set.Set a)
powerset = Set.fromList . Prelude.map Set.fromList . subsequences . Set.toAscList

-- | Calculate the downward closure of a set of sets.
downwardClose :: (Ord a) => Set.Set (Set.Set a) -> Set.Set (Set.Set a)
downwardClose s = Set.foldr Set.union Set.empty $ Set.map powerset s

-- | Check to see if an issue is downward closed
isDownwardClosed :: (Ord a) => Issue a -> Bool
isDownwardClosed i = downwardClose i == i

-- | Creates an empty issue, i.e. an issue that only contains the empty state
emptyIssue :: Issue a
emptyIssue = Set.singleton Set.empty

-- | Create an issue from a list of information states
issue :: (Ord a) => [Set.Set a] -> Issue a
issue [] = emptyIssue
issue (s : ss) = Set.union s' $ issue ss
    where s' = downwardClose $ Set.singleton s

-- | Find the alternatives in an issue
alternatives :: (Ord a) => Issue a -> [Set.Set a]
alternatives i = Set.toList $ Set.filter (\t -> not $ any (Set.isProperSubsetOf t) i) i

-- | A statemap for orderable objects
type StateMap a = Map.Map a (Issue a)

showStateMap :: (Ord a, Show a) => String -> String -> StateMap a -> String
showStateMap i s = intercalate i . Map.elems . Map.mapWithKey show'
    where show' k a = 'S' : s' ++ "(" ++ show k ++ ") = " ++ showIssue a
          s' = if s == "" then "" else '_' : s

showStateMaps :: (Ord a, Show a) => String -> Map.Map Atomic (StateMap a) -> String
showStateMaps i = intercalate i . Map.elems . Map.mapWithKey show'
    where show' k = showStateMap i (show k)
