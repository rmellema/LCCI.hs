{-| This module allows for the reduction of formulas to equivalent formulas that
 - do not use the Update operator.
 -}
module LCCI.Reduce (reduce, reduceStep, tr, k) where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (nub)
import LCCI.Issue (alternatives)
import LCCI.Model
import LCCI.Substitution
import LCCI.Syntax

-- | Reduce all the formulas in a program to an IE-PDL formula
pReduce :: Int -> Program -> Program
pReduce _ (Atom a) = Atom a
pReduce n (Test f) = Test $ reduce n f
pReduce n (Sequence ps) = Sequence $ map (pReduce n) ps
pReduce n (Choice ps) = Choice $ map (pReduce n) ps
pReduce n (Iterate p) = Iterate $ pReduce n p

pre :: UpdateModel -> Event -> Formula
pre u e = precondition u Map.! e

sub :: UpdateModel -> Event -> Substitution
sub u e = substitutions u Map.! e

tr :: UpdateModel -> Set.Set Event -> Set.Set Event -> Program -> Program
tr um s t (Atom a) =
        Sequence [Test $ Or [pre um e | e <- Set.elems s],
                  Choice [Sequence [Test $ npre s s',
                                    Atom a,
                                    Test $ Or [npre t t'
                                              | t' <- Set.elems $ dt s']]
                         | s' <- Set.elems $ Set.filter (not . Set.null) $ Set.powerSet s]]
    where npre s s' = And [Neg $ pre um e | e <- Set.elems $ Set.difference s s']
          sigma = statemap um Map.! a
          smaps s' = Set.unions (Set.map (\e -> sigma Map.! e) s')
          dt s' = Set.filter (\t' -> t' `Set.member` smaps s' && (not . Set.null) t') $ Set.powerSet t
tr um s t (Test f)
    | t `Set.isSubsetOf` s = Test (Update ("", um) (Set.elems s) f)
    | otherwise = Test Bot
tr um s t (Sequence [p]) = tr um s t p
tr um s t (Sequence (p : ps)) =
        Choice [Sequence [tr um s u p, tr um u t (Sequence ps)] |
                u <- Set.elems $ Set.powerSet $ events um]
tr um s t (Choice ps) = Choice [tr um s t p | p <- ps]
tr um s t (Iterate p) = k um s t (events um) p

k :: UpdateModel -> Set.Set Event -> Set.Set Event -> Set.Set Event -> Program -> Program
k um s t u p
    | Set.null u = tr um s t p
    | s == t && t == u =
        Choice [Iterate $ ku s t (d u e) p | e <- Set.elems u]
    | s == u = Sequence [Choice [wt $ Iterate $ ku s u (d u e) p | e <- Set.elems u],
                         Choice [ku u t (d u e) p | e <- Set.elems u]]
    | u == t = Sequence [Choice [ku s u (d u e) p | e <- Set.elems u],
                         Choice [wt $ Iterate $ ku s u (d u e) p | e <- Set.elems u]]
    | otherwise = Choice ([ku s t (d u e) p | e <- Set.elems u] ++ [
                    Sequence [Choice [ku s u (d u e) p | e <- Set.elems u],
                              Choice [wt $ Iterate $ ku u u (d u e) p | e <- Set.elems u],
                              Choice [ku u t (d u e) p | e <- Set.elems u]]])
    where ku = k um
          d u e = u `Set.difference` Set.singleton e
          wt p = Choice [p, Test Top]

reduceUpdate :: Int -> String -> UpdateModel -> [Event] -> Formula -> Formula
reduceUpdate _ _ u es (Prop p) =
        And [Cond (pre u e) (sub u e ! p) | e <- es]
reduceUpdate _ _ u es Bot =
        And [Neg (pre u e) | e <- es]
reduceUpdate _ name u es (And fs) =
        And [Update (name, u) es f | f <- fs]
reduceUpdate _ name u es (IOr fs) =
        IOr [Update (name, u) es f | f <- fs]
reduceUpdate n name u es (Cond a c)
    | isDeclarative (Cond a c) =
        And [Cond (upd [e] a) (upd [e] c) | e <- es]
    | otherwise =
        reduce' n $ IOr $ resolutions n (upd es (Cond a c))
    where upd = Update (name, u)
            -- When a is interrogative we want to do one more reduction step so the implication is completely reduced
          reduce' n = if isDeclarative a then reduceStep n else reduceStep n . reduceStep n
reduceUpdate n name u es (Modal p f) =
        And [Or [And $ nub [Modal (tr u e fs p) $ Update (name, u) (Set.elems fs) a
                           | fs <- Set.elems $ Set.filter (not . Set.null) $ Set.powerSet $ events u]
                | a <- resolutions n f]
            | e <- map Set.singleton es]
reduceUpdate n name u es (IModal p f) =
        And $ nub [IModal (tr u es' fs p) $ Update (name, u) (Set.elems fs) f | fs <- Set.elems $ Set.filter (not . Set.null) $ Set.powerSet $ events u]
    where es' = Set.fromList es
reduceUpdate n name u es (Update (name', u') es' f) =
        Update (name, u) es $ reduceUpdate n name' u' es' f

-- | The inner reduction function that takes an expanded formula and applies the
-- reduction to all of its subformulas, with the exception of Update, which is
-- pushes inwards.
reduceStep' :: Int -> Formula -> Formula
reduceStep' _ (Prop p) = Prop p
reduceStep' _ Bot = Bot
reduceStep' n (And fs) = And $ map (reduceStep' n) fs
reduceStep' n (IOr fs) = IOr $ map (reduceStep' n) fs
reduceStep' n (Cond a c) = Cond (reduceStep' n a) (reduceStep' n c)
reduceStep' n (Modal p f) = Modal (pReduce n p) $ reduceStep' n f
reduceStep' n (IModal p f) = IModal (pReduce n p) $ reduceStep' n f
reduceStep' n (Update (name, u) es f) = reduceUpdate n name u es f

-- | Set the next step of the reduction from LCCI to IE-PDL
reduceStep :: Int -> Formula -> Formula
reduceStep n = reduceStep' n . expand

reduce' :: Int -> Formula -> Formula
reduce' n f
    | f' /= f = reduce n f'
    | otherwise = f
    where f' = reduceStep' n f

-- | Reduce a given LCCI formula to its IE-PDL equivalent
reduce :: Int -> Formula -> Formula
reduce n = reduce' n . expand
