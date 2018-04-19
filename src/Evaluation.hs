{-|
This module holds the code for the evaluation of LCCI formulas in models.
-}
module Evaluation (
    compoundRelation,
    compoundRelation',
    supports,
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Issue
import Model
import Relation
import Syntax

compoundRelation' :: StaticModel -> Program -> Relation
compoundRelation' m (Atom a) = fromMaybe (error msg) (Map.lookup a (relation m))
    where msg = "Atomic relations for '" ++ show a ++ "' not given!"
compoundRelation' m (Test f) = Set.foldl fold Relation.empty ss
    where ss = powerset $ worlds m
          f' = expand f
          test s = supports' m s f' && not (Set.null s)
          fold r s = if test s then Relation.insert r s s else r
compoundRelation' m (Sequence p1 p2) = compose (cr p1) (cr p2)
    where cr = compoundRelation' m
compoundRelation' m (Choice p1 p2) = cr p1 `union` cr p2
    where cr = compoundRelation' m
compoundRelation' m (Iterate p) = reflexiveTransitiveClosure $ compoundRelation' m p
    where reflexiveTransitiveClosure = reflexiveClosure . transitiveClosure


compoundRelation :: StaticModel -> Program -> State -> Set.Set State
compoundRelation m (Atom a) s = Relation.lookup (compoundRelation' m (Atom a)) s
compoundRelation m (Test f) s
    | supports m s f = Set.singleton s
    | otherwise = Set.empty
compoundRelation m (Sequence p1 p2) s = union' $ Set.map seq $ compoundRelation m p1 s
    where seq = compoundRelation m p2
          union' = Set.foldr Set.union Set.empty
compoundRelation m (Choice p1 p2) s = Set.union (cr p1) (cr p2)
    where cr p = compoundRelation m p s
compoundRelation m (Iterate p) s = Relation.lookup r s
    where r = compoundRelation' m p

union' :: Set.Set State -> State
union' = Set.foldr Set.union Set.empty

supports' :: StaticModel -> State -> Formula -> Bool
supports' m s (Prop p) = all lkp s
    where lkp w = Set.member p $ Map.findWithDefault Set.empty w (valuation m)
supports' m s Bot = Set.null s
supports' m s (And f1 f2) = supports' m s f1 && supports' m s f2
supports' m s (IOr f1 f2) = supports' m s f1 || supports' m s f2
supports' m s (Cond f1 f2) = all cond $ powerset s
    where cond t = not (supports' m t f1) || supports' m t f2
supports' m s (Modal p f) = all (\w -> supports' m (t w) f) s
    where t w = union' $ compoundRelation m p $ state [w]
supports' m s (IModal p f) = all (\t -> supports' m t f) $ compoundRelation m p s

supports :: StaticModel -> State -> Formula -> Bool
supports m s f = supports' m s (expand f)
