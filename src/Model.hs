{-|
This module implements both the static and update models of LCCI,
including the Valuation
-}
module Model (
    Valuation,
    ValuationMap,
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

-- | The valuation for an IE-PDL model.
type Valuation a = (Proposition -> a -> Bool)

-- | A simple way to define a Valuation, by specifing which propositions are
-- true in a world.
type ValuationMap a = Map.Map a (Set.Set Proposition)

-- | Turn a @ValuationMap@ into a @Valuation@.
valuationFromMap :: (Ord a) => ValuationMap a -> Valuation a
valuationFromMap m p w = p `Set.member` Map.findWithDefault Set.empty w m

-- | Turn the given @ValuationMap@ into a string.
showValuationMap :: (Show a) => String -> ValuationMap a -> String
showValuationMap s = intercalate s . Map.elems . Map.mapWithKey show'
    where show' k a = "V(" ++ show k ++ ") = " ++ prettyShow a

-- | Turn the given map of @Relation@s into a string.
showRelation :: (PrettyShow a, Ord a) => Map.Map Atomic (Relation a) -> String
showRelation = intercalate "\n" . Map.elems . Map.mapWithKey show'
    where show' k v = "R_{" ++ prettyShow k ++ "} = " ++ prettyShow v

-- | A @StaticModel@ represents an IE-PDL model.
data StaticModel a = StaticModel
                { worlds :: Set.Set a -- ^ The worlds in this model.
                , valuation :: Valuation a -- ^ The valuation for this model. Should be a full function over @worlds@
                , relation :: Map.Map Atomic (Relation a) -- ^ The relation belonging to the model. Should relate every world in @worlds@ to some information state.
                }

instance (Show a) => Show (StaticModel a) where
    show m = "StaticModel " ++ show (worlds m) ++ " undefinedV " ++ show (relation m)

instance (World a, PrettyShow a) => PrettyShow (StaticModel a) where
    prettyShow (StaticModel w v r) = "W = " ++ prettyShow w ++ "\n" ++
                         "V\n" ++
                         showRelation r

-- | A type to represent events in the @UpdateModel@.
newtype Event = Event Int deriving (Ord, Eq, Show)

instance PrettyShow Event where
    prettyShow (Event e) = 'e' : show e

instance (World a, Ord b) => World (a, b)

-- | A type to represent an LCCI model, used to be combined with a @StaticModel@
-- to evaluate @Formula@s with an update operator.
data UpdateModel = UpdateModel
                { events :: Set.Set Event
                , statemap :: Map.Map Atomic (StateMap Event)
                , precondition :: Map.Map Event Formula
                , substitutions :: Map.Map Event Substitution
                } deriving (Eq, Show)

-- | Turn the preconditions into a string representation that is human readable.
showPreconditions :: Map.Map Event Formula -> String
showPreconditions = intercalate "\n" . Map.elems . Map.mapWithKey f
    where f k v = "pre(" ++ prettyShow k ++ ") = " ++ show v

-- | Turn the substitutions into a string representation that is human readable.
showSubstitutions :: Map.Map Event Substitution -> String
showSubstitutions = intercalate "\n" . Map.elems . Map.mapWithKey f
    where f k v = "sub(" ++ prettyShow k ++ ") = " ++ show v

instance PrettyShow UpdateModel where
    prettyShow (UpdateModel es s pre sub) = "E = " ++ prettyShow es ++ "\n" ++
                                      showStateMaps "\n" s ++ "\n" ++
                                      showPreconditions pre ++ "\n" ++
                                      showSubstitutions sub

