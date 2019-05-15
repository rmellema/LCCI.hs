{-|
A module that holds classes and functions for transforming a given
datastructure into its (La)TeX representation. This module assumes things
will be printed in mathmode, unless specifically mentioned otherwise or
enabled by the resulting LaTeX code itself. It will also assume the
excistens of the following LaTeX macros:

 [@\\lnot@] Logical negation

 [@\\land@] Logical conjunction

 [@\\lor@] Logical disjunction

 [@\\lior@] Inquisitive disjunction

 [@\\lif@] Material conditional

 [@\\liff@] Biimplication

 [@\\lac@] Typesets the modal program operator. Takes one argument, which is
            the program that goes into the modal operator.

 [@\\liac@] Typesets the inquisitive modal program operator. Takes one
            argument, which is the program that goes into the modal operator.

 [@\\lup@] Typesets the update operator. Takes an optional argument (the name
            of the update model) and a required one (the (set of) events that
            is being executed.

 [@\\upd@] Typesets in the font used for update model symbols. Takes one argument, the thing to be typeset in that font.

 [@\\pre@] Typesets the function for the precondition. Takes one argument, the thing upon which the function is called.

 [@\\sub@] Typesets the function for the substitution. Takes one argument, the thing upon which the function is called.
 -}
module Tex where
import Data.List (intercalate)
import Issue
import Model
import Syntax
import Util

-- | Convert a datastructure to a valid TeX representation of the structure.
-- May use the commands given at the start of this module.
class Texable a where
    -- | Create a TeX representation of the given object, prepended with the
    -- given string on each line, put in braces.
    texWithParen :: String -> a -> String
    texWithParen s x = s ++ toTexWithParen x
    -- | Create a TeX representation of the given object, prepend with the
    -- given string on each line
    texPrepend :: String -> a -> String
    texPrepend s x = s ++ toTex x
    -- | Create a TeX representation of the given subject, if needed within
    -- brackets
    toTexWithParen :: a -> String
    toTexWithParen x = '(' : toTex x ++")"
    -- | Create a TeX representation of a given object
    toTex :: a -> String
    toTex = texPrepend ""

-- | Enclose the TeX representation of the list with the first and second
-- argument, separated by the third. Useful for making lists and sets.
enclose :: (Texable a) => String -> String -> String -> [a] -> String
enclose l r s x = l ++ intercalate s (map toTexWithParen x) ++ r

-- | Get the TeX representation enclosed in parenthesis. The first argument is
-- the separator for in between the elements.
withParens :: (Texable a) => String -> [a] -> String
withParens = enclose "(" ")"

asSet :: (Texable a) => [a] -> String
asSet = enclose "\\{" "\\}" ", "

instance Texable Program where
    toTexWithParen (Atom a) = prettyShow a
    toTexWithParen p@(Iterate _) = toTex p
    toTexWithParen p = '(' : toTex p ++ ")"

    toTex (Atom a) = prettyShow a
    toTex (Test f) = '?' : toTexWithParen f
    toTex (Sequence ps) = intercalate " ; " $ map toTexWithParen ps
    toTex (Choice ps) = intercalate " \\cup " $ map toTexWithParen ps
    toTex (Iterate p) = '{' : toTexWithParen p ++ "}^*"

instance Texable Formula where
    toTexWithParen f@(Prop _) = toTex f
    toTexWithParen Bot = toTex Bot
    toTexWithParen Top = toTex Top
    toTexWithParen f@(Neg _) = toTex f
    toTexWithParen f@(Quest _) = toTex f
    toTexWithParen (And fs) = withParens " \\land " fs
    toTexWithParen (Or fs) = withParens " \\lor " fs
    toTexWithParen (IOr fs) = withParens " \\lior " fs
    toTexWithParen (Cond a c) = withParens " \\lif " [a, c]
    toTexWithParen (BiCond a c) = withParens " \\liff " [a, c]
    toTexWithParen f@(Modal _ _) = toTex f
    toTexWithParen f@(IModal _ _) = toTex f
    toTexWithParen f@Update{} = toTex f

    toTex (Prop p) = prettyShow p
    toTex Bot = "\\bot"
    toTex Top = "\\top"
    toTex (Neg f) = texWithParen "\\lnot" f
    toTex (Quest f) = texWithParen "?" f
    toTex (And fs) = intercalate " \\land " $ map toTexWithParen fs
    toTex (Or fs) = intercalate " \\lor " $ map toTexWithParen fs
    toTex (IOr fs) = intercalate " \\lior " $ map toTexWithParen fs
    toTex (Cond a c) = toTexWithParen a ++ " \\lif " ++ toTexWithParen c
    toTex (BiCond a c) = toTexWithParen a ++ " \\liff " ++ toTexWithParen c
    toTex (Modal p f) = texWithParen ("\\lac{" ++ toTexWithParen p ++ "}") f
    toTex (IModal p f) = texWithParen ("\\liac{" ++ toTexWithParen p ++ "}") f
    toTex (Update (s, _) es f) = texWithParen ("\\lup[" ++ s ++ "]{" ++ asSet es ++ "}") f

instance Texable Event where
    toTexWithParen = toTex
    toTex (Event n) = "\\upd{e_" ++ show n ++ "}"
