module Util (
    permutate,
    prettyShowWithParen,
    PrettyShow,
    prettyShow,
    prettyPrint,
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
