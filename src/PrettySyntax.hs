{-|
 - A module for making the reading and writing of formulas easier
-}
module PrettySyntax (
    PrettyShow,
    prettyShow,
    module PrettySyntax,
) where
import Syntax
import Evaluation
import Model
import Issue
import Util (PrettyShow, prettyShow)

class ProgramLike a where
    asProgram :: a -> Program

instance ProgramLike Program where
    asProgram = id

instance ProgramLike Atomic where
    asProgram = Atom

test :: (FormulaLike a) => a -> Program
test = Test . asFormula

infixl 5 .>
(.>) :: (ProgramLike a, ProgramLike b) => a -> b -> Program
p1 .> p2 = flattenStep $ Choice [asProgram p1, asProgram p2]

infix 4 .^
(.^) :: (ProgramLike a) => a -> Int -> Program
p .^ n = Sequence $ replicate n (asProgram p)

infixl 5 \/
(\/) :: (ProgramLike a, ProgramLike b) => a -> b -> Program
p1 \/ p2 = flattenStep $ Choice [asProgram p1, asProgram p2]

iter :: (ProgramLike a) => a -> Program
iter = Iterate . asProgram

class FormulaLike a where
    asFormula :: a -> Formula

instance FormulaLike Formula where
    asFormula = id

instance FormulaLike Proposition where
    asFormula = Prop

polar :: (FormulaLike a) => a -> Formula
polar f = IOr [f', Neg f']
    where f' = asFormula f

neg :: (FormulaLike a) => a -> Formula
neg = Neg . asFormula

infixl 5 &
(&) :: (FormulaLike a, FormulaLike b) => a -> b -> Formula
f1 & f2 = flattenStep $ And [asFormula f1, asFormula f2]

infixl 5 !|
(!|) :: (FormulaLike a, FormulaLike b) => a -> b -> Formula
f1 !| f2 = flattenStep $ Or [asFormula f1, asFormula f2]

infixl 5 ?|
(?|) :: (FormulaLike a, FormulaLike b) => a -> b -> Formula
f1 ?| f2 = flattenStep $ IOr [asFormula f1, asFormula f2]

infix 6 -->
(-->) :: Formula -> Formula -> Formula
(-->) = Cond
infix 6 <->
(<->) :: Formula -> Formula -> Formula
(<->) = BiCond

knows :: (ProgramLike a, FormulaLike b) => a -> b -> Formula
knows p f = Modal (asProgram p) (asFormula f)

entertains :: (ProgramLike a, FormulaLike b) => a -> b -> Formula
entertains p f = IModal (asProgram p) (asFormula f)

infix 1 @@
(@@) :: (World a) => StaticModel a -> UpdateModel -> StaticModel (a, Event)
(@@) = productUpdate

infix 1 .@
(.@) :: (World a) => (StaticModel a, State a) -> (UpdateModel, [Event]) -> State (a, Event)
(.@) (m, s) (u, es) = updatedState m s u es

infix 9 |=
(|=) :: (World a) => (StaticModel a, State a) -> Formula -> Bool
(m, s) |= f = supports m s f
