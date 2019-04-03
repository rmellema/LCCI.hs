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
        flattenStep,
        flatten,
        expandStep,
        expand,
        isDeclarative,
        resolutions
) where
import Data.Char(toLower, toUpper)
import Data.List(nub)
import Data.Maybe (fromJust)
import Util(permutate, showWithParen)

-- | A datatype for structures that can be flattened, like programs and formulas
class (Eq a) => FlattenAble a where
    flattenStep :: a -> a
    flatten :: a -> a
    flatten f
        | f' /= f = flatten f'
        | otherwise = f
        where f' = flattenStep f

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

instance FlattenAble Program where
    flattenStep (Atom a) = Atom a
    flattenStep (Test f) = Test $ flatten f
    flattenStep (Sequence [p]) = p
    flattenStep (Sequence ps) = Sequence (nub $ concatMap unpack ps)
        where unpack (Sequence subps) = map flattenStep ps
              unpack p = [p]
    flattenStep (Choice ps) = Choice (nub $ concatMap unpack ps)
        where unpack (Choice subps) = map flattenStep ps
              unpack p = [p]
    flattenStep (Iterate p) = Iterate $ flatten p


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

instance FlattenAble Formula where
    -- | Flattens a formula by one level
    flattenStep (Neg f) = Neg $ flattenStep f
    flattenStep (And []) = Top
    flattenStep (And [f]) = f
    flattenStep (And fs) = And (nub $ concatMap unpack fs)
        where unpack (And subfs) = map flattenStep subfs
              unpack f = [f]
    flattenStep (Or []) = Bot
    flattenStep (Or [f]) = f
    flattenStep (Or fs) = Or (nub $ concatMap unpack fs)
        where unpack (Or subfs) = map flattenStep subfs
              unpack f = [f]
    flattenStep (IOr fs) = IOr (nub $ concatMap unpack fs)
        where unpack (IOr subfs) = map flattenStep subfs
              unpack f = [f]
    flattenStep (Cond f1 f2) = Cond (flattenStep f1) (flattenStep f2)
    flattenStep (BiCond f1 f2) = BiCond (flattenStep f1) (flattenStep f2)
    flattenStep (Modal p f) = Modal p $ flattenStep f
    flattenStep (IModal p f) = IModal p $ flattenStep f
    flattenStep f = f

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
expandStep (And fs) = And (map expandStep fs)
expandStep (Or []) = Bot
expandStep (Or [f]) = expandStep f
expandStep (Or fs) = Neg $ And $ map Neg fs
expandStep (IOr []) = Bot
expandStep (IOr [f]) = expandStep f
expandStep (IOr fs) = IOr (map expandStep fs)
expandStep (Cond f g) = Cond (expandStep f) (expandStep g)
expandStep (BiCond f g) = And [Cond f g, Cond g f]
expandStep (Modal p f) = Modal p (expandStep f)
expandStep (IModal p f) = IModal p (expandStep f)

-- | Expand a formula by replacing all of the abbreviations in them.
expand :: Formula -> Formula
expand f
    | f' /= f = expand f'
    | otherwise = f
    where f' = expandStep $ flatten f

isDeclarativeProgram :: Program -> Bool
isDeclarativeProgram (Atom _) = True
isDeclarativeProgram (Test f) = False
isDeclarativeProgram (Sequence ps) = isDeclarativeProgram $ head ps
isDeclarativeProgram (Choice ps) = all isDeclarativeProgram ps
isDeclarativeProgram (Iterate p) = isDeclarativeProgram p

innerIsDeclarative :: Formula -> Bool
innerIsDeclarative (Prop _) = True
innerIsDeclarative Bot = True
innerIsDeclarative (And fs) = all innerIsDeclarative fs
innerIsDeclarative (IOr _) = False
innerIsDeclarative (Cond _ f) = innerIsDeclarative f
innerIsDeclarative (Modal _ _) = True
innerIsDeclarative (IModal p f) = isDeclarativeProgram p || innerIsDeclarative f

-- | Check if a given formula is declarative
isDeclarative :: Formula -> Bool
isDeclarative = innerIsDeclarative . expand

-- | Calculate all the resolutions of a given formula
calcRes :: Formula -> [Formula]
calcRes (And fs) = [And ys | ys <- permutate $ map resolutions fs]
calcRes (IOr fs) = concatMap resolutions fs
calcRes (Cond a c) = [And [Cond a' (fromJust $ lookup a' f) | a' <- ra] | f <- fs]
    where ra = resolutions a
          rc = resolutions c
          fs = permutate [[(x, y) | y <- rc] | x <- ra]
calcRes (IModal (Test f) c) = resolutions (Cond f c)
calcRes (IModal (Sequence ps) f) = resolutions (IModal (head ps) (IModal (Sequence $ tail ps) f))
calcRes (IModal (Choice ps) f) = resolutions (And [IModal p f | p <- ps])
calcRes (IModal (Iterate p) f) = undefined

-- | Give the resolutions of a given formula
resolutions :: Formula -> [Formula]
resolutions f
    | isDeclarative f = [f]
    | otherwise       = calcRes $ expandStep f
