{-|
This module implements both the static and update models of LCCI,
including the Valuation
-}
module Model (
    Valuation,
    showValuation,
    showRelation,
    StaticModel(..),
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List(intercalate, subsequences)
import Issue
import Relation
import Syntax

-- | A valuation for a LCCI model
type Valuation = Map.Map World (Set.Set Proposition)

showValuation :: String -> Valuation -> String
showValuation s = intercalate s . Map.elems . Map.mapWithKey show'
    where show' k a = "V(" ++ show k ++ ") = " ++ showState a

showRelation :: Map.Map Atomic Relation -> String
showRelation = intercalate "\n" . Map.elems . Map.mapWithKey show'
    where show' k v = "R_{" ++ show k ++ "} = " ++ show v

data StaticModel = StaticModel
                { worlds :: Set.Set World
                , valuation :: Valuation
                , relation :: Map.Map Atomic Relation
                }

instance Show StaticModel where
    show (StaticModel w v r) = "W = " ++ showState w ++ "\n" ++
                         showValuation "\n" v ++ "\n" ++
                         showRelation r
