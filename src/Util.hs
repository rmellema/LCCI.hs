{-|
This Module exports some utility functions that are in use in other parts of the
project.
-}
module Util (
    permutate,
    prettyShowWithParen,
    PrettyShow,
    prettyShow,
    prettyPrint,
    takeUntil,
    match,
) where
import Data.Foldable
import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude hiding (foldr)

-- | Given a list of lists, calculate all the possible permutations of their
-- elements. So it picks one element for each list, and then puts that in front
-- of the permutations of the rest of the lists. This is the same as making a
-- list comprehension that takes from each list, but for an unknown amount of
-- lists.
permutate :: [[a]] -> [[a]]
permutate [] = []
permutate [xs] = [[x] | x <- xs]
permutate (xs:xss) = [x:ys | x <- xs, ys <- permutate xss]

-- | A class for objects that can be shown in a "pretty" way, instead of just
-- in a way that Haskell can read in.
class PrettyShow a where
    -- | Show an object in a more human-readable manner
    prettyShow :: a -> String

instance PrettyShow Bool where
    prettyShow = show

instance (PrettyShow a, PrettyShow b) => PrettyShow (a, b) where
    prettyShow (a, b) = '(' : prettyShow a ++ ", " ++ prettyShow b ++ ")"

instance (PrettyShow a) => PrettyShow [a] where
    prettyShow = show . map prettyShow

instance (PrettyShow a) => PrettyShow (Set.Set a) where
    prettyShow s = "{" ++ intercalate ", " (Set.toAscList $ Set.map prettyShow s) ++ "}"

instance (PrettyShow a, PrettyShow b) => PrettyShow (Map.Map a b) where
    prettyShow = intercalate "\n" . Map.elems . Map.mapWithKey (\k v ->
        prettyShow k ++ " |-> " ++ prettyShow v)


-- | Show a list of objects with parenthesis around it. The first argument is
-- the string that is shown in between the elements of the list
prettyShowWithParen :: (PrettyShow a) => String -> [a] -> String
prettyShowWithParen i xs = '(' : intercalate i (map prettyShow xs) ++ ")"

-- | Same as `print`, but using `prettyShow` instead of `show`.
prettyPrint :: (PrettyShow a) => a -> IO ()
prettyPrint = putStrLn . prettyShow

-- | Take from a list until a certain predicate @p@ holds. This includes the first
-- element for which the predicate holds.
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
    | not $ p x = x : takeUntil p xs
    | otherwise = [x]

-- | Calculate the matching starts for a list of lists, and return the mathing
-- start together with the non-matching ends of the lists.
match :: (Eq a) => [[a]] -> ([a], [[a]])
match xs = (m, map (drop $ length m) xs)
    where m = foldr1 f xs
          f a x = [p1 | (p1, p2) <- zip a x, p1 == p2]
