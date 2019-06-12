{-|
 - A module for making the reading and writing of formulas easier, by defining
 - operators and functions that are less efficient, but eaier to use.
-}
module LCCI.Syntax.Pretty (
    PrettyShow,
    prettyShow,
    module LCCI.Syntax.Pretty,
) where
import LCCI.Syntax
import LCCI.Evaluation
import LCCI.Model
import LCCI.Issue
import LCCI.Util (PrettyShow, prettyShow)

-- | A class for objects that can be cast into programs.
class ProgramLike a where
    -- | Cast an object into a program
    asProgram :: a -> Program

instance ProgramLike Program where
    asProgram = id

instance ProgramLike Atomic where
    asProgram = Atom

-- | Create a test program for a given formula
test :: (FormulaLike a) => a -> Program
test = Test . asFormula

-- | The sequence operator for programs
infixl 5 .>
(.>) :: (ProgramLike a, ProgramLike b) => a -> b -> Program
p1 .> p2 = flattenStep $ Sequence [asProgram p1, asProgram p2]

-- | The power operator for programs
infix 4 .^
(.^) :: (ProgramLike a) => a -> Int -> Program
p .^ n = Sequence $ replicate n (asProgram p)

-- | The choice operators for programs
infixl 5 .+
(.+) :: (ProgramLike a, ProgramLike b) => a -> b -> Program
p1 .+ p2 = flattenStep $ Choice [asProgram p1, asProgram p2]

-- | Create the iterate program of a given program
iter :: (ProgramLike a) => a -> Program
iter = Iterate . asProgram

-- | A class fo objects that can be cast into Formulas
class FormulaLike a where
    -- | Cast an object into a formula
    asFormula :: a -> Formula

instance FormulaLike Formula where
    asFormula = id

instance FormulaLike Proposition where
    asFormula = Prop

instance FormulaLike Bool where
    asFormula True = Top
    asFormula False = Bot

-- | Create a polar question for a given formula
polar :: (FormulaLike a) => a -> Formula
polar f = IOr [f', Neg f']
    where f' = asFormula f

-- | Give the negation of a given formula
neg :: (FormulaLike a) => a -> Formula
neg = Neg . asFormula

-- | The conjunction operator for formulas
infixl 5 /\
(/\) :: (FormulaLike a, FormulaLike b) => a -> b -> Formula
f1 /\ f2 = flattenStep $ And [asFormula f1, asFormula f2]

-- | The disjunction operator for formulas
infixl 5 \/
(\/) :: (FormulaLike a, FormulaLike b) => a -> b -> Formula
f1 \/ f2 = flattenStep $ Or [asFormula f1, asFormula f2]

-- | The inquisitive disjunction operator for formulas
infixl 5 \\/
(\\/) :: (FormulaLike a, FormulaLike b) => a -> b -> Formula
f1 \\/ f2 = flattenStep $ IOr [asFormula f1, asFormula f2]

-- | The implication operator
infix 6 -->
(-->) :: (FormulaLike a, FormulaLike b) => a -> b -> Formula
a --> c = flattenStep $ Cond (asFormula a) (asFormula c)
-- | Biimplication operator
infix 6 <->
(<->) :: (FormulaLike a, FormulaLike b) => a -> b -> Formula
a <-> c = flattenStep $ BiCond (asFormula a) (asFormula c)

-- | The normal modality
knows :: (ProgramLike a, FormulaLike b) => a -> b -> Formula
knows p f = Modal (asProgram p) (asFormula f)

-- | The inquisitive modality
entertains :: (ProgramLike a, FormulaLike b) => a -> b -> Formula
entertains p f = IModal (asProgram p) (asFormula f)

-- | A function to quickly make the wonder modality
wonder :: (ProgramLike a, FormulaLike b) => a -> b -> Formula
wonder p f = neg (knows p f) /\ entertains p f

-- | The update procedure operator
infix 1 @@
(@@) :: (World a) => StaticModel a -> UpdateModel -> StaticModel (a, Event)
(@@) = productUpdate

-- | The updated state operator
infix 1 .@
(.@) :: (World a) => (StaticModel a, State a) -> (UpdateModel, [Event]) -> State (a, Event)
(.@) (m, s) (u, es) = updatedState m s u es

-- | Supports operator
infix 9 |=
(|=) :: (World a) => (StaticModel a, State a) -> Formula -> Bool
(m, s) |= f = supports m s f
