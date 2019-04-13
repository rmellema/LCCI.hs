{-|
A module that defines the substitutions for LCCI and functions for working with
them. A substitution is a map from propositional atoms to formulas, that can be
used to change the truth value of a proposition after the execution of an
action.
-}
module Substitution (
    Substitution,
    null,
    size,
    empty,
    insert,
    fromList,
    toList,
    domain,
    lookup,
    apply,
    (!),
) where
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (lookup, null)
import Syntax
import Util

-- | A type for the substitutions.
newtype Substitution = Substitution (Map.Map Proposition Formula)
    deriving (Eq, Show)

instance PrettyShow Substitution where
    prettyShow (Substitution s) = "{" ++ intercalate ", " [g p f | (p, f) <- Map.toList s] ++ "}"
        where g p f = prettyShow p ++ " |-> " ++ prettyShow f

-- | Test to see if a substitution is empty
null :: Substitution -> Bool
null (Substitution s) = Map.null s

-- | Calculate the size of a given substitution, i.e. how many propositions it
-- maps to other formulas.
size :: Substitution -> Int
size (Substitution s) = Map.size s

-- | The empty substitution.
empty :: Substitution
empty = Substitution Map.empty

-- | Insert a new mapping into a substitution.
insert :: Proposition -> Formula -> Substitution -> Substitution
insert p f (Substitution s) = Substitution (Map.insert p f s)

-- | Create a substitution from a list of mappings.
fromList :: [(Proposition, Formula)] -> Substitution
fromList [] = empty
fromList ((p, f) : xs) = insert p f $ fromList xs

-- | Transform the given substitution into a list of mappings.
toList :: Substitution -> [(Proposition, Formula)]
toList (Substitution s) = Map.toList s

-- | The domain of this substitution.
domain :: Substitution -> [Proposition]
domain (Substitution s) = Map.keys $ Map.filterWithKey f s
    where f k (Prop p) = k == p
          f _ _ = False

-- | Lookup a given proposition in the substitution. Returns `Nothing` if the
-- proposition is not mapped to anything else.
lookup :: Proposition -> Substitution -> Maybe Formula
lookup p (Substitution s) = Map.lookup p s

-- | Lookup the given proposition in the substitution. Returns the proposition as a formula in case it is not mapped to anything.
infixl 9 !
(!) :: Substitution -> Proposition -> Formula
s ! p = fromMaybe (Prop p) $ lookup p s

-- | Apply a substitution to a program
pApply s (Atom a) = Atom a
pApply s (Test f) = Test $ apply s f
pApply s (Sequence ps) = Sequence $ map (pApply s) ps
pApply s (Choice ps) = Choice $ map (pApply s) ps
pApply s (Iterate p) = Iterate $ pApply s p

-- | Apply the proposition to the substitution. The same as `lookup`, but
-- returns the proposition in case it is not mapped to anything.
apply :: Substitution -> Formula -> Formula
apply s (Prop p) = s ! p
apply s Bot = Bot
apply s Top = Top
apply s (Neg f) = Neg $ apply s f
apply s (Quest f) = Quest $ apply s f
apply s (And fs) = And $ map (apply s) fs
apply s (Or fs) = Or $ map (apply s) fs
apply s (IOr fs) = IOr $ map (apply s) fs
apply s (Cond a c) = Cond (apply s a) (apply s c)
apply s (BiCond a c) = BiCond (apply s a) (apply s c)
apply s (Modal p f) = Modal (pApply s p) (apply s f)
apply s (IModal p f) = IModal (pApply s p) (apply s f)
