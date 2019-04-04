module Util (
    permutate,
    showWithParen,
    PrettyShow,
    prettyShow,
) where
import Data.Foldable
import Data.List (intercalate)
import qualified Data.Set as Set
import Prelude hiding (foldr)

permutate :: [[a]] -> [[a]]
permutate [] = []
permutate [xs] = [[x] | x <- xs]
permutate (xs:xss) = [x:ys | x <- xs, ys <- permutate xss]

showWithParen :: (Show a) => String -> [a] -> String
showWithParen i xs = '(' : intercalate i (map show xs) ++ ")"

class PrettyShow a where
    prettyShow :: a -> String

instance (PrettyShow a, PrettyShow b) => PrettyShow (a, b) where
    prettyShow (a, b) = '(' : prettyShow a ++ ", " ++ prettyShow b ++ ")"

instance (PrettyShow a) => PrettyShow (Set.Set a) where
    prettyShow s = "{" ++ intercalate ", " (Set.toAscList $ Set.map prettyShow s) ++ "}"
