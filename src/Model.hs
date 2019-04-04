{-|
This module implements both the static and update models of LCCI,
including the Valuation
-}
module Model (
    Valuation,
    showValuationMap,
    showRelation,
    StaticModel(..),
    Event(..),
    UpdateModel(..),
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List(intercalate, subsequences)
import Issue
import Relation
import Substitution
import Syntax
import Util

-- | A valuation for a LCCI model
type ValuationMap a = Map.Map a (Set.Set Proposition)

type Valuation a = (Proposition -> a -> Bool)

valuationFromMap :: (Ord a) => ValuationMap a -> Valuation a
valuationFromMap m p w = p `Set.member` Map.findWithDefault Set.empty w m

showValuationMap :: (Show a) => String -> ValuationMap a -> String
showValuationMap s = intercalate s . Map.elems . Map.mapWithKey show'
    where show' k a = "V(" ++ show k ++ ") = " ++ prettyShow a

showRelation :: (PrettyShow a, Ord a) => Map.Map Atomic (Relation a) -> String
showRelation = intercalate "\n" . Map.elems . Map.mapWithKey show'
    where show' k v = "R_{" ++ show k ++ "} = " ++ prettyShow v

data StaticModel a = StaticModel
                { worlds :: Set.Set a
                , valuation :: Valuation a
                , relation :: Map.Map Atomic (Relation a)
                }

instance (World a, PrettyShow a) => PrettyShow (StaticModel a) where
    prettyShow (StaticModel w v r) = "W = " ++ prettyShow w ++ "\n" ++
                         "V\n" ++
                         showRelation r

newtype Event = Event Int deriving (Ord, Eq, Show)

instance PrettyShow Event where
    prettyShow (Event e) = 'e' : show e

instance (World a, Ord b) => World (a, b)

data UpdateModel = UpdateModel
                { events :: Set.Set Event
                , statemap :: Map.Map Atomic (StateMap Event)
                , precondition :: Map.Map Event Formula
                , substitutions :: Map.Map Event Substitution
                } deriving Eq

showPreconditions :: Map.Map Event Formula -> String
showPreconditions = intercalate "\n" . Map.elems . Map.mapWithKey f
    where f k v = "pre(" ++ show k ++ ") = " ++ show v

showSubstitutions :: Map.Map Event Substitution -> String
showSubstitutions = intercalate "\n" . Map.elems . Map.mapWithKey f
    where f k v = "sub(" ++ show k ++ ") = " ++ show v

instance Show UpdateModel where
    show (UpdateModel es s pre sub) = "E = " ++ prettyShow es ++ "\n" ++
                                      showStateMaps "\n" s ++ "\n" ++
                                      showPreconditions pre ++ "\n" ++
                                      showSubstitutions sub

