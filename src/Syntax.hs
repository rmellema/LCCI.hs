{-|
This module implements the Syntax of LCCI, and formulas for manipulating this
syntax.
-}
module Syntax (
        Atomic,
        atom,
        Program(..),
        Proposition,
        proposition,
        Formula(..),
        expand
) where
import Data.Char(toLower, toUpper)

-- | An atomic program with no internal structure
newtype Atomic = Atomic String deriving (Ord, Eq)

instance Show Atomic where
    show (Atomic name) = name

-- | Create a new atom from a string. The first letter will be lowercased
atom :: String -> Atomic
atom [] = error "Empty atom name given!"
atom (s:ss) = Atomic (toLower s : ss)

-- | A full program
data Program = Atom Atomic
             | Test Formula
             | Sequence Program Program
             | Choice Program Program
             | Iterate Program
    deriving Eq

instance Show Program where
    show (Atom a) = show a
    show (Test f) = show f ++ "?"
    show (Sequence p1 p2) = "(" ++ show p1 ++ ";" ++ show p2 ++ ")"
    show (Choice p1 p2) = "(" ++ show p1 ++ " u " ++ show p2 ++ ")"
    show (Iterate p) = show p ++ "*"

-- | A Logical Proposition
newtype Proposition = Proposition String deriving (Ord, Eq)

instance Show Proposition where
    show (Proposition name) = name

-- | Create a new Proposition from a string. The first letter will be uppercased
proposition :: String -> Proposition
proposition [] = error "Empty proposition name given!"
proposition (s:ss) = Proposition (toUpper s : ss)

-- | A Formula in LCCI
data Formula = Prop Proposition
             | Bot
             | Neg Formula
             | Quest Formula
             | And Formula Formula
             | Or Formula Formula
             | IOr Formula Formula
             | Cond Formula Formula
             | BiCond Formula Formula
             | Modal Program Formula
             | IModal Program Formula
    deriving Eq

instance Show Formula where
    show (Prop p) = show p
    show Bot = "_|_"
    show (Neg f) = "!" ++ show f
    show (Quest f) = "?" ++ show f
    show (And f1 f2) = "(" ++ show f1 ++ " & " ++ show f2 ++ ")"
    show (Or f1 f2) = "(" ++ show f1 ++ " | " ++ show f2 ++ ")"
    show (IOr f1 f2) = "(" ++ show f1 ++ " \\| " ++ show f2 ++ ")"
    show (Cond f1 f2) = "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"
    show (BiCond f1 f2) = "(" ++ show f1 ++ " <-> " ++ show f2 ++ ")"
    show (Modal p f) = "[" ++ show p ++ "] " ++ show f
    show (IModal p f) = "[[" ++ show p ++ "]] " ++ show f

-- | Expand a formula by replacing all of the abbreviations in them.
expand :: Formula -> Formula
expand (Prop p) = Prop p
expand Bot = Bot
expand (Neg f) = Cond (expand f) Bot
expand (Quest f) = IOr f' (expand $ Neg f')
    where f' = f
expand (And f1 f2) = And (expand f1) (expand f2)
expand (Or f1 f2) = expand $ Neg (And (Neg (expand f1)) (Neg (expand f2)))
expand (IOr f1 f2) = IOr (expand f1) (expand f2)
expand (Cond f1 f2) = Cond (expand f1) (expand f2)
expand (BiCond f1 f2) = And (Cond f1' f2') (Cond f2' f1')
    where f1' = expand f1
          f2' = expand f2
expand (Modal p f) = Modal p (expand f)
expand (IModal p f) = IModal p (expand f)
