{-|
An implementation of the famous Hex model in LCCI.
-}
module Hex where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Model
import Syntax

-- | The worlds in the Hex model
w1, w2, w3 :: World
w1 = World 1
w2 = World 2
w3 = World 3
w4 = World 4
w5 = World 5
w6 = World 6

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

-- | Each world as a formula
a0b1c2, a0b2c1, a1b0c2,a1b2c0,a2b0c1,a2b1c0 :: Formula
a0b1c2 = And (Prop a0) (And (Prop b1) (Prop c2))
a0b2c1 = And (Prop a0) (And (Prop b2) (Prop c1))
a1b0c2 = And (Prop a1) (And (Prop b0) (Prop c2))
a1b2c0 = And (Prop a1) (And (Prop b2) (Prop c0))
a2b0c1 = And (Prop a2) (And (Prop b0) (Prop c1))
a2b1c0 = And (Prop a2) (And (Prop b1) (Prop c0))

-- | A list with all worlds as formulas
forms = [a0b1c2, a0b2c1, a1b0c2, a1b2c0, a2b0c1, a2b1c0]

-- | The 'actions' in the Hex model (The knowledge relations)
a, b, c :: Atomic
a = atom "a"
b = atom "b"
c = atom "c"

-- | The set of worlds
ws :: Set.Set World
ws = Set.fromList [w1, w2, w3, w4, w5, w6]

-- | The valuation of the propositional atoms
v :: Valuation
v = Map.fromList
    [ (w1, Set.fromList [a0, b1, c2])
    , (w2, Set.fromList [a0, b2, c1])
    , (w3, Set.fromList [a1, b0, c2])
    , (w4, Set.fromList [a1, b2, c0])
    , (w5, Set.fromList [a2, b0, c1])
    , (w6, Set.fromList [a2, b1, c0])
    ]

-- | The StateMaps for the various actions
s :: Map.Map Atomic StateMap
s = Map.fromList
    [ (a, Map.fromList
        [ (w1, issue [state [w1], state [w2]])
        , (w2, issue [state [w1], state [w2]])
        , (w3, issue [state [w3], state [w4]])
        , (w4, issue [state [w3], state [w4]])
        , (w5, issue [state [w5], state [w6]])
        , (w6, issue [state [w5], state [w6]])
        ])
    , (b, Map.fromList
        [ (w1, issue [state [w1], state [w6]])
        , (w2, issue [state [w2], state [w4]])
        , (w3, issue [state [w3], state [w5]])
        , (w4, issue [state [w2], state [w4]])
        , (w5, issue [state [w3], state [w5]])
        , (w6, issue [state [w1], state [w6]])
        ])
    , (c, Map.fromList
        [ (w1, issue [state [w1], state [w3]])
        , (w2, issue [state [w2], state [w5]])
        , (w3, issue [state [w1], state [w3]])
        , (w4, issue [state [w4], state [w6]])
        , (w5, issue [state [w2], state [w5]])
        , (w6, issue [state [w4], state [w6]])
        ])
    ]

-- | The actual model
m :: Model
m = Model ws v s
