{-|
This module holds the code for the evaluation of LCCI formulas in models.
-}
module Evaluation (
    stateMap,
    supports,
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Model
import Syntax

all' :: Set.Set Bool -> Bool
all' = Set.foldr (&&) True

errMsg :: Atomic -> World -> String
errMsg a w = "Issue for program " ++ show a ++ " from " ++ show w ++ " undefined"

lookupWorld :: (World -> String) -> Maybe StateMap -> World -> Issue
lookupWorld f (Just s) w = fromMaybe (error $ f w) (Map.lookup w s)
lookupWorld f Nothing w = error $ f w

union :: (Ord a) => Set.Set (Set.Set a) -> Set.Set a
union = Set.unions . Set.elems

iterMap :: Model -> Issue -> Program -> [(World, State)] -> Set.Set (World, State) -> Issue
iterMap _ acc _ []  _ = acc
iterMap m acc p ((w, s) : ws) ds = iterMap m acc' p ws' ds'
    where acc' = Set.union acc ts
          ws'  = filter (\x -> not $ Set.member x ds') $ concat $ Set.elems $ Set.map (\t -> Set.elems $ Set.map (\v -> (v, t)) t) ts
          ds'  = Set.insert (w, s) ds
          ts   = stateMap m p w s

stateMap :: Model -> Program -> World -> State -> Issue
stateMap m (Atom a) w _ = lookupWorld (errMsg a) (Map.lookup a (stateMaps m)) w
stateMap m (Test f) w s
    | supports m s f = issue [s]
    | otherwise    = error (show f ++ " not supported in " ++ showState s)
stateMap m (Sequence p1 p2) w s = union $ Set.map (\v -> stateMap m p2 v sp1) sp1
    where sp1 = union $ stateMap m p1 w s
stateMap m (Choice p1 p2) w s = Set.union (stateMap m p1 w s) (stateMap m p2 w s)
stateMap m (Iterate p) w s = iterMap m (issue [state [w]]) p [(w, s)] Set.empty

imodal :: Model -> State -> Program -> Formula -> World -> Bool
imodal m s p f w = all' $ Set.map (\t -> supports m t f) $ stateMap m p w s

supports' :: Model -> State -> Formula -> Bool
supports' m s (Prop p) = all' $ Set.map lkp s
    where lkp w = Set.member p $ Map.findWithDefault Set.empty w (valuation m)
supports' m s Bot = Set.null s
supports' m s (And f1 f2) = supports' m s f1 && supports' m s f2
supports' m s (IOr f1 f2) = supports' m s f1 || supports' m s f2
supports' m s (Cond f1 f2) = all' $ Set.map cond $ powerset s
    where cond t = not (supports' m t f1) || supports' m t f2
supports' m s (Modal p f) = all' $ Set.map h s
    where h w = supports' m (union $ stateMap m p w s) f
supports' m s (IModal p f) = all' $ Set.map (imodal m s p f) s

supports :: Model -> State -> Formula -> Bool
supports m s f = supports' m s (expand f)
