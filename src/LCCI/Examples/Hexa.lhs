\section{The Hex Game}
An implementation of the famous Hex model in LCCI.

\begin{code}
module LCCI.Examples.Hex where
import qualified Data.Map as Map
import qualified Data.Set as Set
import LCCI.Issue
import LCCI.Model
import LCCI.Relation as Relation
import LCCI.Syntax
import LCCI.PrettySyntax
\end{code}

\begin{code}
instance (Ord a, Ord b, Ord c) => World (a, b, c)

instance (Show a, Show b, Show c) => PrettyShow (a, b, c) where
    prettyShow (a, b, c) = show a ++ show b ++ show c

-- | The worlds in the Hex model
w1, w2, w3, w4, w5, w6 :: (Int, Int, Int)
w1 = (0, 1, 2)
w2 = (0, 2, 1)
w3 = (1, 0, 2)
w4 = (1, 2, 0)
w5 = (2, 0, 1)
w6 = (2, 1, 0)

-- | The propositions in the Hex model
a0, a1, a2, b0, b1, b2, c0, c1, c2 :: Proposition
a0 = proposition "A0"
a1 = proposition "A1"
a2 = proposition "A2"
b0 = proposition "B0"
b1 = proposition "B1"
b2 = proposition "B2"
c0 = proposition "C0"
c1 = proposition "C1"
c2 = proposition "C2"

-- | Get the number of the card for the given proposition.
card :: Proposition -> String
card = tail . prettyShow

-- | Get the identifier for the player in the given proposition.
player :: Proposition -> Char
player = head . prettyShow

-- | Each world as a formula
a0b1c2, a0b2c1, a1b0c2,a1b2c0,a2b0c1,a2b1c0 :: Formula
a0b1c2 = a0 /\ b1 /\ c2
a0b2c1 = a0 /\ b2 /\ c1
a1b0c2 = a1 /\ b0 /\ c2
a1b2c0 = a1 /\ b2 /\ c0
a2b0c1 = a2 /\ b0 /\ c1
a2b1c0 = a2 /\ b1 /\ c0

-- | A list with all worlds as formulas
forms = [a0b1c2, a0b2c1, a1b0c2, a1b2c0, a2b0c1, a2b1c0]

-- | The "actions" in the Hex model (The knowledge relations)
a, b, c :: Atomic
a = atom "a"
b = atom "b"
c = atom "c"

-- | The set of worlds
ws :: Set.Set (Int, Int, Int)
ws = Set.fromList [w1, w2, w3, w4, w5, w6]

-- | The valuation of the propositional atoms
v :: Valuation (Int, Int, Int)
v p (ca, cb, cc) = show (card' (player p)) == card p
    where card' 'A' = ca
          card' 'B' = cb
          card' 'C' = cc

-- | The relations for all the various agents.
r :: Map.Map Atomic (Relation (Int, Int, Int))
r = Map.fromList
    [ (a, Relation.fromList
        [ (state [w1], state [w1]), (state [w1], state [w2])
        , (state [w2], state [w1]), (state [w2], state [w2])
        , (state [w3], state [w3]), (state [w3], state [w4])
        , (state [w4], state [w3]), (state [w4], state [w4])
        , (state [w5], state [w5]), (state [w5], state [w6])
        , (state [w6], state [w5]), (state [w6], state [w6])
        ])
    , (b, Relation.fromList
        [ (state [w1], state [w1]), (state [w1], state [w6])
        , (state [w2], state [w2]), (state [w2], state [w4])
        , (state [w3], state [w3]), (state [w3], state [w5])
        , (state [w4], state [w2]), (state [w4], state [w4])
        , (state [w5], state [w5]), (state [w5], state [w3])
        , (state [w6], state [w1]), (state [w6], state [w6])
        ])
    , (c, Relation.fromList
        [ (state [w1], state [w1]), (state [w1], state [w3])
        , (state [w2], state [w2]), (state [w2], state [w5])
        , (state [w3], state [w3]), (state [w3], state [w1])
        , (state [w4], state [w4]), (state [w4], state [w6])
        , (state [w5], state [w5]), (state [w5], state [w2])
        , (state [w6], state [w6]), (state [w6], state [w4])
        ])
    ]

-- | The actual model
hex :: StaticModel (Int, Int, Int)
hex = StaticModel ws v r
\end{code}
