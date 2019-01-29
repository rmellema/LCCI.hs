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
        expandStep,
        expand
) where
import Data.Char(toLower, toUpper)
import Data.List(nub)
import Util(showWithParen)

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
             | Sequence [Program]
             | Choice [Program]
             | Iterate Program
    deriving Eq

instance Show Program where
    show (Atom a) = show a
    show (Test f) = '?' : show f
    show (Sequence ps) = showWithParen "; " ps
    show (Choice ps) = showWithParen " u " ps
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
             | Top
             | Neg Formula
             | Quest Formula
             | And [Formula]
             | Or [Formula]
             | IOr [Formula]
             | Cond Formula Formula
             | BiCond Formula Formula
             | Modal Program Formula
             | IModal Program Formula
    deriving Eq

instance Show Formula where
    show (Prop p) = show p
    show Bot = "_|_"
    show Top = "T"
    show (Neg f) = "!" ++ show f
    show (Quest f) = "?" ++ show f
    show (And fs) = showWithParen " & " fs
    show (Or fs) = showWithParen " | " fs
    show (IOr fs) = showWithParen " \\| " fs
    show (Cond f1 f2) = "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"
    show (BiCond f1 f2) = "(" ++ show f1 ++ " <-> " ++ show f2 ++ ")"
    show (Modal p f) = "[" ++ show p ++ "] " ++ show f
    show (IModal p f) = "[[" ++ show p ++ "]] " ++ show f

-- | Expand the outer part of a formula by replacing the abbreviations. Also
-- folds together conjunctions inside conjunctions etc.
expandStep :: Formula -> Formula
expandStep (Prop p) = Prop p
expandStep Bot = Bot
expandStep Top = Neg Bot
expandStep (Neg f) = Cond f Bot
expandStep (Quest f) = IOr [f, Neg f]
expandStep (And []) = Top
expandStep (And [f]) = expandStep f
expandStep (And fs) = And (nub $ concatMap unpack fs)
    where unpack (And subfs) = map expandStep subfs
          unpack f = [expandStep f]
expandStep (Or []) = Bot
expandStep (Or [f]) = expandStep f
expandStep (Or fs) = Neg $ And $ map Neg fs
expandStep (IOr []) = Bot
expandStep (IOr [f]) = expandStep f
expandStep (IOr fs) = IOr (nub $ concatMap unpack fs)
    where unpack (IOr subfs) = map expandStep subfs
          unpack f = [expandStep f]
expandStep (Cond f g) = Cond (expandStep f) (expandStep g)
expandStep (BiCond f g) = And [Cond f g, Cond g f]
expandStep (Modal p f) = Modal p (expandStep f)
expandStep (IModal p f) = IModal p (expandStep f)

-- | Expand a formula by replacing all of the abbreviations in them.
expand :: Formula -> Formula
expand f
    | f' /= f = expand f'
    | otherwise = f
    where f' = expandStep f
