{-|
This module holds the code for the evaluation of LCCI formulas in models.
-}
module Evaluation (
    compoundRelation,
    compoundRelation',
    supports,
    productUpdate,
    updatedState,
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import Issue
import Model
import Relation
import Substitution hiding (lookup)
import Syntax

compoundRelation' :: (World a) => StaticModel a -> Program -> Relation a
compoundRelation' m (Atom a) = fromMaybe (error msg) (Map.lookup a (relation m))
    where msg = "Atomic relations for '" ++ show a ++ "' not given!"
compoundRelation' m (Test f) = Set.foldl fold Relation.empty ss
    where ss = powerset $ worlds m
          fold r s = Set.foldl (\r' t -> Relation.insert r' s t) r $ compoundRelation m (Test f) s
compoundRelation' m (Sequence ps) = foldr (compose . cr) Relation.empty ps
    where cr = compoundRelation' m
compoundRelation' m (Choice ps) = foldr (union . cr) Relation.empty ps
    where cr = compoundRelation' m
compoundRelation' m (Iterate p) = transitiveClosure $ compoundRelation' m p

compoundRelation :: (World a) => StaticModel a -> Program -> State a -> Set.Set (State a)
compoundRelation m (Atom a) s = Relation.lookup (compoundRelation' m (Atom a)) s
compoundRelation m (Test f) s = Set.filter (\t -> supports m t f) $ Set.powerSet s
compoundRelation m (Sequence ps) s = foldr (\p ts -> Set.unions (Set.map (cr p) ts)) (Set.singleton s) ps
    where cr = compoundRelation m
compoundRelation m (Choice ps) s = foldr (Set.union . cr) Set.empty ps
    where cr p = compoundRelation m p s
compoundRelation m (Iterate p) s = Relation.lookup r s
    where r = compoundRelation' m p

union' :: (Ord a) => Set.Set (State a) -> State a
union' = Set.foldr Set.union Set.empty

proj :: (World a, Ord b) => State (a, b) -> (State a, Set.Set b)
proj s = (Set.map fst s, Set.map snd s)

proj1 :: (World a, Ord b) => State (a, b) -> State a
proj1 = fst . proj

proj2 :: (World a, Ord b) => State (a, b) -> Set.Set b
proj2 = snd . proj

pre :: UpdateModel -> Event -> Formula
pre u e = precondition u Map.! e

sub :: UpdateModel -> Event -> Substitution
sub u e = substitutions u Map.! e

relUpdate :: (World a) => State (a, Event) -> Atomic -> StaticModel a -> UpdateModel -> Relation (a, Event)
relUpdate ws a m u = Relation.fromList
        [(state [(w, e)], t) | (w, e) <- Set.toList ws, t <- Set.toList $ Set.powerSet ws,
                               proj1 t `Set.member` lookup ra (state [w]),
                               proj2 t `Set.member` (sa Map.! e)]
        where ra = relation m Map.! a
              sa = statemap u Map.! a

productUpdate :: (World a) => StaticModel a -> UpdateModel -> StaticModel (a, Event)
productUpdate m u = StaticModel ws v r
    where ws = Set.fromList [(w, e) | w <- Set.toList $ worlds m,
                                      e <- Set.toList $ events u,
                                    supports m (state [w]) $ pre u e]
          v p (w, e) = supports m (state [w]) (sub u e ! p)
          r = Map.fromList [(a, relUpdate ws a m u) | a <- Map.keys (relation m)]

updatedState :: (World a) => StaticModel a -> State a -> UpdateModel -> [Event] -> State (a, Event)
updatedState m s u es = state [(w, e) | w <- Set.toList s,
                                       e <- es,
                                        supports m (state[w]) $ pre u e]

supports' :: (World a) => StaticModel a -> State a -> Formula -> Bool
supports' m s (Prop p) = all (val p) s
    where val = valuation m
supports' m s Bot = Set.null s
supports' m s (And fs) = all (supports' m s) fs
supports' m s (IOr fs) = any (supports' m s) fs
supports' m s (Cond f1 f2) = all cond $ powerset s
    where cond t = not (supports' m t f1) || supports' m t f2
supports' m s (Modal p f) = all (\w -> supports' m (t w) f) s
    where t w = union' $ compoundRelation m p $ state [w]
supports' m s (IModal p f) = all (\t -> supports' m t f) $ compoundRelation m p s
supports' m s (Update (_, u) es f) =
                supports (productUpdate m u) (updatedState m s u es) f

supports :: (World a) => StaticModel a -> State a -> Formula -> Bool
supports m s f = supports' m s (expand f)
