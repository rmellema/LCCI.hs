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
        FlattenAble,
        flattenStep,
        flatten,
        Simplify,
        simplifyStep,
        simplify,
        expandStep,
        expand,
        isDeclarative,
        resolutions
) where
import Data.Char(toLower, toUpper)
import Data.List(nub)
import Data.Maybe (fromJust)
import Util
import {-# SOURCE #-} Model

-- | A datatype for structures that can be flattened, like programs and formulas
class (Eq a) => FlattenAble a where
    -- | Flatten a given structure by one step in the process.
    flattenStep :: a -> a
    -- | Fully flatten the given structure.
    flatten :: a -> a
    flatten f
        | f' /= f = flatten f'
        | otherwise = f
        where f' = flattenStep f

-- | A datatype for structures that can be flattenend an simplified, like
-- programs and formulas.
class (FlattenAble a) => Simplify a where
    -- | Simplify the given structure by one step in the simplification process
    simplifyStep :: a -> a
    -- | Fully simplify the given structure.
    simplify :: a -> a
    simplify x
        | x' /= x = simplify x'
        | otherwise = x
        where x' = simplifyStep $ flatten x

-- | An atomic program with no internal structure
newtype Atomic = Atomic String deriving (Ord, Eq, Show)

instance PrettyShow Atomic where
    prettyShow (Atomic name) = name

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
    deriving (Eq, Show)

instance PrettyShow Program where
    prettyShow (Atom a) = prettyShow a
    prettyShow (Test f) = '?' : prettyShow f
    prettyShow (Sequence ps) = prettyShowWithParen "; " ps
    prettyShow (Choice ps) = prettyShowWithParen " u " ps
    prettyShow (Iterate p) = prettyShow p ++ "*"

instance FlattenAble Program where
    flattenStep (Atom a) = Atom a
    flattenStep (Test f) = Test $ flatten f
    flattenStep (Sequence [p]) = p
    flattenStep (Sequence ps) = Sequence (nub $ concatMap unpack ps)
        where unpack (Sequence subps) = map flattenStep subps
              unpack p = [flattenStep p]
    flattenStep (Choice [p]) = p
    flattenStep (Choice ps) = Choice (nub $ concatMap unpack ps)
        where unpack (Choice subps) = map flattenStep subps
              unpack p = [flattenStep p]
    flattenStep (Iterate p) = Iterate $ flatten p

instance Simplify Program where
    simplifyStep (Atom a) = Atom a
    simplifyStep (Test f) = Test $ simplify f
    simplifyStep (Sequence ps)
        -- If this sequence contains ?_|_, then we can stop there.
        | Test Bot `elem` ps = Sequence $ takeUntil (== Test Bot) ps
        | Test Top `elem` ps && last ps /= Test Top = Sequence $ filter (/= Test Top) ps
        | otherwise = Sequence $ map simplifyStep ps
    simplifyStep (Choice ps)
        -- If everything ends with ?_|_, then just return that
        | all f ps = Test Bot
        -- Any (sequence that ends with) ?_|_ can be removed, since those don't add anything
        | any f ps = Choice $ filter (not . f) ps
        | Test Bot `elem` ps && any (/= Test Bot) ps = Choice $ filter (/= Test Bot) ps
        -- If we have p u p*, then the first p does not add anything
        | any (\p -> any (initer p) ps) ps =
            Choice $ filter (\p -> not $ any (initer p) ps) ps
        -- If all the starts in the sequence are the same, then we can combine the starts
        | all isSeq ps && not (null leadSeq) && all (not . null) tailSeqs =
            Sequence (leadSeq ++ [Choice $ map flatten [Sequence ps' | ps' <- tailSeqs]])
        -- If all the ends in the sequence are the same, then we can combine the ends
        | all isSeq ps && not (null endSeq) && all (not . null) beginSeqs =
            Sequence ((Choice $ map flatten [Sequence $ reverse ps' | ps' <- beginSeqs]) : reverse endSeq)
        | otherwise = Choice $ nub $ map simplifyStep ps
        where isSeq (Sequence _) = True
              isSeq _            = False
              initer p (Iterate p') = p == p'
              initer _ _ = False
              f (Sequence ps) = last ps == Test Bot
              f (Test Bot) = True
              f _ = False
              matches = match $ map (\(Sequence p) -> p) ps
              leadSeq = fst matches
              tailSeqs = snd matches
              revmatches = match $ map (\(Sequence p) -> reverse p) ps
              endSeq = fst revmatches
              beginSeqs = snd revmatches
    simplifyStep (Iterate (Test f)) = Test f
    simplifyStep (Iterate p) = Iterate $ simplifyStep p

-- | A Logical Proposition
newtype Proposition = Proposition String deriving (Ord, Eq, Show)

instance PrettyShow Proposition where
    prettyShow (Proposition name) = name

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
             | Update (String, UpdateModel) [Event] Formula
    deriving (Eq, Show)

instance PrettyShow Formula where
    prettyShow (Prop p) = prettyShow p
    prettyShow Bot = "_|_"
    prettyShow Top = "T"
    prettyShow (Neg f) = "!" ++ prettyShow f
    prettyShow (Quest f) = "?" ++ prettyShow f
    prettyShow (And fs) = prettyShowWithParen " /\\ " fs
    prettyShow (Or fs) = prettyShowWithParen " \\/ " fs
    prettyShow (IOr fs) = prettyShowWithParen " \\\\/ " fs
    prettyShow (Cond f1 f2) = "(" ++ prettyShow f1 ++ " --> " ++ prettyShow f2 ++ ")"
    prettyShow (BiCond f1 f2) = "(" ++ prettyShow f1 ++ " <-> " ++ prettyShow f2 ++ ")"
    prettyShow (Modal p f) = "[" ++ prettyShow p ++ "] " ++ prettyShow f
    prettyShow (IModal p f) = "[[" ++ prettyShow p ++ "]] " ++ prettyShow f
    prettyShow (Update (n, _) e f) = "[" ++ n ++ ", " ++ prettyShow e ++ "] " ++ prettyShow f

instance FlattenAble Formula where
    -- | Flattens a formula by one level
    flattenStep (Neg f) = Neg $ flattenStep f
    flattenStep (And []) = Top
    flattenStep (And [f]) = f
    flattenStep (And fs) = And (nub $ concatMap unpack fs)
        where unpack (And subfs) = map flattenStep subfs
              unpack f = [flattenStep f]
    flattenStep (Or []) = Bot
    flattenStep (Or [f]) = f
    flattenStep (Or fs) = Or (nub $ concatMap unpack fs)
        where unpack (Or subfs) = map flattenStep subfs
              unpack f = [flattenStep f]
    flattenStep (IOr []) = Bot
    flattenStep (IOr [f]) = f
    flattenStep (IOr fs) = IOr (nub $ concatMap unpack fs)
        where unpack (IOr subfs) = map flattenStep subfs
              unpack f = [flattenStep f]
    flattenStep (Cond f1 f2) = Cond (flattenStep f1) (flattenStep f2)
    flattenStep (BiCond f1 f2) = BiCond (flattenStep f1) (flattenStep f2)
    flattenStep (Modal p f) = Modal (flattenStep p) $ flattenStep f
    flattenStep (IModal p f) = IModal (flattenStep p) $ flattenStep f
    flattenStep (Update m [] f) = Top
    flattenStep (Update m e f) = Update m e $ flattenStep f
    flattenStep f = f

-- | We can transform a formula into a structuraly similar and logically
-- equivalent formula.
instance Simplify Formula where
    simplifyStep (Neg Bot) = Top
    simplifyStep (Neg Top) = Bot
    simplifyStep (Neg (Neg f))
        | isDeclarative f = f
        | otherwise       = Neg $ simplifyStep $ Neg f
    simplifyStep (Neg f  ) = Neg $ simplifyStep f
    simplifyStep (And fs)
        | Bot `elem` fs = Bot
        | Top `elem` fs = flatten $ And $ filter (/=Top) fs
        | otherwise = flatten $ And $ nub $ map simplifyStep fs
    simplifyStep (Or fs)
        | Top `elem` fs = Top
        | Bot `elem` fs = flatten $ Or $ filter (/= Bot) fs
        | otherwise = flatten $ Or $ nub $ map simplifyStep fs
    simplifyStep (IOr fs)
        | Top `elem` fs = Top
        | Bot `elem` fs = flatten $ IOr $ filter (/= Bot) fs
        | otherwise = flatten $ IOr $ nub $ map simplifyStep fs
    simplifyStep (Cond a Bot) = Neg a
    simplifyStep (Cond a c) = Cond (simplifyStep a) (simplifyStep c)
    simplifyStep (BiCond a c) = BiCond (simplifyStep a) (simplifyStep c)
    simplifyStep (Modal _ Top) = Top
    simplifyStep (Modal p@(Sequence ps) f)
        | last ps == Test Bot = Top
        | otherwise = Modal (simplifyStep p) (simplifyStep f)
    simplifyStep (Modal p f)
        | p == Test Bot = Top
        | otherwise = Modal (simplifyStep p) (simplifyStep f)
    simplifyStep (IModal _ Top) = Top
    simplifyStep (IModal p@(Sequence ps) f)
        | last ps == Test Bot = Top
        | otherwise = IModal (simplifyStep p) (simplifyStep f)
    simplifyStep (IModal p f)
        | p == Test Bot = Top
        | otherwise = IModal (simplifyStep p) (simplifyStep f)
    simplifyStep (Update u e f)
        | null e = Top
        | otherwise = Update u e $ simplifyStep f
    simplifyStep f = f

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
expandStep (Update m e f) = Update m e $ expandStep f

-- | Expand a formula by replacing all of the abbreviations in them.
expand :: Formula -> Formula
expand f
    | f' /= f = expand f'
    | otherwise = f
    where f' = expandStep $ flatten f

-- | Returns if a given program is a declarative program, i.e., if `IModal p f`
-- is declarative for any @f@
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
innerIsDeclarative (Update _ _ f) = innerIsDeclarative f

-- | Check if a given formula is declarative
isDeclarative :: Formula -> Bool
isDeclarative = innerIsDeclarative . expand

-- | Calculate all the resolutions of a given formula
calcRes :: Int -> Formula -> [Formula]
calcRes n (And fs) = [And ys | ys <- permutate $ map (resolutions n) fs]
calcRes n (IOr fs) = concatMap (resolutions n) fs
calcRes n (Cond a c) = [And [Cond a' (fromJust $ lookup a' f) | a' <- ra] | f <- fs]
    where ra = resolutions n a
          rc = resolutions n c
          fs = permutate [[(x, y) | y <- rc] | x <- ra]
calcRes n (IModal (Test f) c) = resolutions n (Cond f c)
calcRes n (IModal (Sequence ps) f) = resolutions n (IModal (head ps) (IModal (Sequence $ tail ps) f))
calcRes n (IModal (Choice ps) f) = resolutions n (And [IModal p f | p <- ps])
calcRes n (IModal (Iterate p) f) = resolutions n $ And [IModal (Sequence $ replicate m p) f | m <- [1..(2^n)]]
calcRes n (Update m e f) = [Update m e a | a <- resolutions n f]

-- | Give the resolutions of a given formula for models with at most `Int`
-- worlds.
resolutions :: Int -> Formula -> [Formula]
resolutions n f
    | isDeclarative f = [f]
    | otherwise       = calcRes n $ expandStep f
