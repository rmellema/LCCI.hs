module Event (
    Event,
    StateMap,
    showStateMap,
    showStateMaps,
) where
import Data.List (intercalate)
import qualified Data.Map as Map
import Issue
import Syntax

newtype Event = Event Int deriving (Ord, Eq)

instance Show Event where
    show (Event e) = 'e' : show e

-- | A statemap for atomic programs
type StateMap a = Map.Map Event (Issue a)

showStateMap :: (Show a) => String -> String -> StateMap a -> String
showStateMap i s = intercalate i . Map.elems . Map.mapWithKey show'
    where show' k a = 'S' : s' ++ "(" ++ show k ++ ") = " ++ showIssue a
          s' = if s == "" then "" else '_' : s

showStateMaps :: (Show a) => String -> Map.Map Atomic (StateMap a) -> String
showStateMaps i = intercalate i . Map.elems . Map.mapWithKey show'
    where show' k = showStateMap i (show k)
