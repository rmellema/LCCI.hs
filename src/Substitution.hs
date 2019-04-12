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

-- | A type for the substitutions
newtype Substitution = Substitution (Map.Map Proposition Formula)
    deriving (Eq, Show)

instance PrettyShow Substitution where
    prettyShow (Substitution s) = "{" ++ intercalate ", " [g p f | (p, f) <- Map.toList s] ++ "}"
        where g p f = prettyShow p ++ " |-> " ++ prettyShow f

null :: Substitution -> Bool
null (Substitution s) = Map.null s

size :: Substitution -> Int
size (Substitution s) = Map.size s

empty :: Substitution
empty = Substitution Map.empty

insert :: Proposition -> Formula -> Substitution -> Substitution
insert p f (Substitution s) = Substitution (Map.insert p f s)

fromList :: [(Proposition, Formula)] -> Substitution
fromList [] = empty
fromList ((p, f) : xs) = insert p f $ fromList xs

toList :: Substitution -> [(Proposition, Formula)]
toList (Substitution s) = Map.toList s

domain :: Substitution -> [Proposition]
domain (Substitution s) = Map.keys $ Map.filterWithKey f s
    where f k (Prop p) = k == p
          f _ _ = False
lookup :: Proposition -> Substitution -> Maybe Formula
lookup p (Substitution s) = Map.lookup p s

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
