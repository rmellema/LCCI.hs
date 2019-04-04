{-| This module allows for the reduction of formulas to equivalent formulas that
 - do not use the Update operator.
 -}
module Reduce (reduce, reduceStep) where
import qualified Data.Map as Map
import Model
import Substitution
import Syntax

-- | Reduce all the formulas in a program to an IE-PDL formula
pReduce :: Program -> Program
pReduce (Atom a) = Atom a
pReduce (Test f) = Test $ reduce f
pReduce (Sequence ps) = Sequence $ map pReduce ps
pReduce (Choice ps) = Choice $ map pReduce ps
pReduce (Iterate p) = Iterate $ pReduce p

pre :: UpdateModel -> Event -> Formula
pre u e = precondition u Map.! e

sub :: UpdateModel -> Event -> Substitution
sub u e = substitutions u Map.! e

t :: UpdateModel -> [Event] -> [Event] -> Program -> Program
t um s t (Atom a) = undefined
t um s t (Test f) = undefined
t um s t (Sequence ps) = undefined
t um s t (Choice ps) = undefined
t um s t (Iterate p) = undefined

k :: UpdateModel -> [Event] -> [Event] -> [Event] -> Program -> Program
k um s t u p = undefined

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
reduceStep' n (Modal p f) = Modal (pReduce p) $ reduceStep' n f
reduceStep' n (IModal p f) = IModal (pReduce p) $ reduceStep' n f
reduceStep' n (Update (name, u) es f) = reduceUpdate n name u es f

-- | Set the next step of the reduction from LCCI to IE-PDL
reduceStep :: Int -> Formula -> Formula
reduceStep n = reduceStep' n . expand

reduce' = undefined

-- | Reduce a given LCCI formula to its EI-PDL equivalent
reduce :: Formula -> Formula
reduce = reduce' . expand
