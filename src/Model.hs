{-|
This module implements both the static and update models of LCCI,
including the Valuation
-}
module Model (
    Valuation,
    StateMap,
    showStateMap,
    Model(..),
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List(intercalate, subsequences)
import Issue
import Syntax

-- | A valuation for a LCCI model
type Valuation = Map.Map World (Set.Set Proposition)

showValuation :: String -> Valuation -> String
showValuation s = intercalate s . Map.elems . Map.mapWithKey show'
    where show' k a = "V(" ++ show k ++ ") = " ++ showState a

showStateMaps :: String -> Map.Map Atomic StateMap -> String
showStateMaps i = intercalate i . Map.elems . Map.mapWithKey show'
    where show' k = showStateMap i (show k)

data StaticModel = StaticModel
                { worlds :: Set.Set World
                , valuation :: Valuation
                , stateMaps :: Map.Map Atomic StateMap
                }

instance Show StaticModel where
    show (StaticModel w v r) = "W = " ++ showState w ++ "\n" ++
                         showValuation "\n" v ++ "\n" ++
                         showStateMaps "\n" s
