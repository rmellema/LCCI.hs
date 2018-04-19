{-|
This module implements the structures from Inquisitive Semantics
Information States, Issues, and State Maps.
-}
module Issue (
    World(..),
    State,
    showState,
    state,
    Issue,
    showIssue,
    emptyIssue,
    issue,
    powerset,
    downwardClose,
    isDownwardClosed,
    StateMap,
    showStateMap,
    showStateMaps,
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List(intercalate, subsequences)
import Syntax

-- | A world in an LCCI model
newtype World = World Int deriving (Ord, Eq)

instance Show World where
    show (World w) = 'w' : show w

-- | An information state
type State = Set.Set World

-- | A prettier way to show Information States
showState :: (Show a) => Set.Set a -> String
showState s = '{' : intercalate ", " (Set.toAscList $ Set.map show s) ++ "}"

-- | A convience function for defining States. Creates a state with every
-- world in the input list
state :: [World] -> State
state = Set.fromList

-- | A new issue. This will not garantee that an Issue is downward closed.
type Issue = Set.Set State

-- | A prettier way to show Issues
showIssue :: Issue -> String
showIssue s = '{' : intercalate ", " (Set.toAscList $ Set.map showState s) ++ "}"

-- | Create the powerset of a given set.
powerset :: (Ord a) => Set.Set a -> Set.Set (Set.Set a)
powerset = Set.fromList . Prelude.map Set.fromList . subsequences . Set.toAscList

-- | Calculate the downward closure of a set of sets.
downwardClose :: (Ord a) => Set.Set (Set.Set a) -> Set.Set (Set.Set a)
downwardClose s = Set.foldr Set.union Set.empty $ Set.map powerset s

-- | Check to see if an issue is downward closed
isDownwardClosed :: Issue -> Bool
isDownwardClosed i = downwardClose i == i

emptyIssue :: Issue
emptyIssue = Set.singleton Set.empty

-- | Create an issue from a list of information states
issue :: [State] -> Issue
issue [] = emptyIssue
issue (s : ss) = Set.union s' $ issue ss
    where s' = downwardClose $ Set.singleton s

-- | A statemap for atomic programs
type StateMap = Map.Map World Issue

showStateMap :: String -> String -> StateMap -> String
showStateMap i s = intercalate i . Map.elems . Map.mapWithKey show'
    where show' k a = 'S' : s' ++ "(" ++ show k ++ ") = " ++ showIssue a
          s' = if s == "" then "" else '_' : s

showStateMaps :: String -> Map.Map Atomic StateMap -> String
showStateMaps i = intercalate i . Map.elems . Map.mapWithKey show'
    where show' k = showStateMap i (show k)
