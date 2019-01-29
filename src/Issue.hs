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
type Issue a = Set.Set (State a)

-- | A prettier way to show Issues
showIssue :: (Show a) => Issue a -> String
showIssue s = '{' : intercalate ", " (Set.toAscList $ Set.map showSet s) ++ "}"

-- | Create the powerset of a given set.
powerset :: (Ord a) => Set.Set a -> Set.Set (Set.Set a)
powerset = Set.fromList . Prelude.map Set.fromList . subsequences . Set.toAscList

-- | Calculate the downward closure of a set of sets.
downwardClose :: (Ord a) => Set.Set (Set.Set a) -> Set.Set (Set.Set a)
downwardClose s = Set.foldr Set.union Set.empty $ Set.map powerset s

-- | Check to see if an issue is downward closed
isDownwardClosed :: (Ord a) => Issue a -> Bool
isDownwardClosed i = downwardClose i == i

emptyIssue :: Issue a
emptyIssue = Set.singleton Set.empty

-- | Create an issue from a list of information states
issue :: (Ord a) => [State a] -> Issue a
issue [] = emptyIssue
issue (s : ss) = Set.union s' $ issue ss
    where s' = downwardClose $ Set.singleton s
