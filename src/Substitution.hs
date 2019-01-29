module Substitution (
    Substitution,
    null,
    size,
    empty,
    insert,
    fromList,
    toList,
) where
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (lookup, null)
import Syntax

-- | A type for the substitutions
newtype Substitution = Substitution (Map.Map Proposition Formula)
    deriving (Eq)

instance Show Substitution where
    show (Substitution s) = "{" ++ intercalate ", " [g p f | (p, f) <- Map.toList s] ++ "}"
        where g p f = show p ++ " |-> " ++ show f

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

lookup :: Proposition -> Substitution -> Maybe Formula
lookup p (Substitution s) = Map.lookup p s

apply :: Proposition -> Substitution -> Formula
apply p s = fromMaybe (Prop p) $ lookup p s
