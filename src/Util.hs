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
import Prelude hiding (foldr)

permutate :: [[a]] -> [[a]]
permutate [] = []
permutate [xs] = [[x] | x <- xs]
permutate (xs:xss) = [x:ys | x <- xs, ys <- permutate xss]

prettyShowWithParen :: (PrettyShow a) => String -> [a] -> String
prettyShowWithParen i xs = '(' : intercalate i (map prettyShow xs) ++ ")"

class PrettyShow a where
    prettyShow :: a -> String

instance PrettyShow Bool where
    prettyShow = show

instance (PrettyShow a, PrettyShow b) => PrettyShow (a, b) where
    prettyShow (a, b) = '(' : prettyShow a ++ ", " ++ prettyShow b ++ ")"

instance (PrettyShow a) => PrettyShow [a] where
    prettyShow = show . map prettyShow

instance (PrettyShow a) => PrettyShow (Set.Set a) where
    prettyShow s = "{" ++ intercalate ", " (Set.toAscList $ Set.map prettyShow s) ++ "}"

prettyPrint :: (PrettyShow a) => a -> IO ()
prettyPrint = putStrLn . prettyShow

-- | Take from a list until a certain predicate `p` holds. This includes the first
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
