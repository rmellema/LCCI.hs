module Util (
    permutate,
    showWithParen,
    showSet
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

showSet :: (Show a) => Set.Set a -> String
showSet s = "{" ++ intercalate ", " (Set.toAscList $ Set.map show s) ++ "}"
