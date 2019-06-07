\section{The Hexa Game}
In this example, we will walk through the modeling of the Hexa game in \LCCI,
using the implementation to aid us in answering questions about the model. The
Hexa game was first presented in~\parencite{vanDitmarsch:2000vw}, but is more
often used as a testbed for Dynamic Epistemic Logics. We decided to use this as
an example instead of Citadells since the models are smaller and easier to
understand.

Since the implementation is written in Haskell, the example will also be given
in Haskell. While we will try to keep the example easy to follow for those who
have no experience with Haskell, if you want to use the implementation
yourself, it might be useful to read up on using the language. For this we
would recommend using \citetitle{Doets:2012uk} \parencite{Doets:2012uk}. If you
are also interested in using Haskell as a general programming language, then
\citetitle{lyah} \parencite{lyah} is also a good introduction.

\subsection{The Game}
Before we can start to model the game in \LCCI, we will first have to
understand the rules of the game. In the Hexa game, there are three players,
which we will call Ann ($a$), Bill ($b$), and Carol ($c$). Each of these
players will get a unique card out of a deck of three. We will number the cards
0, 1, and 2. The goal of the game is to know the full deal of the cards by
asking questions and getting answers.

The only types of questions that we will allow are asking for a specific card.
To make the example more interesting and informative, we will also allow for
different actions, such as the swapping of cards between agents.

\subsection{Setting up}
Before we can start to model the example, we will first have to set up some
Haskell specific things. In Haskell code is normally organized in modules,
which are named after where they are in the file system. Since this files lives
in the \texttt{LCCI/Examples} folder and is named \texttt{Hexa.lhs}, the module
name will be \lstinline|LCCI.Examples.Hexa|.

\begin{code}
module LCCI.Examples.Hexa where
\end{code}

Since the implementation did not want to reinvent the wheel, we will also need
to import some other packages, in particular packages for sets and maps. These
come from the collections module from Hackage.

\begin{code}
import qualified Data.Map as Map
import qualified Data.Set as Set
\end{code}

Now we can import the \LCCI\ specific packages that we need. The first of these
is the \lstinline|LCCI.Issue| package, which contains code for representing
information states, issues, and state maps.
\begin{code}
import LCCI.Issue
\end{code}

We will also need to represent the models. The \lstinline|LCCI.Model| package
contains code for working with and define both the static models from \IEPDL\
and the update models from \LCCI\@.
\begin{code}
import LCCI.Model
\end{code}

Besides needing to represent the models, we will also need to represent
formulas of the language, so we can ask questions about them to the
implementation. For this, we will need to import the \lstinline|LCCI.Syntax|
package.
\begin{code}
import LCCI.Syntax
\end{code}

And lastly, because \lstinline|LCCI.Syntax| defines the syntax in a way that is
not as easy to read or write as one might want, we will import the
\lstinline|LCCI.Syntax.Pretty| package to use functions that are a bit easier
on the eyes.
\begin{code}
import LCCI.Syntax.Pretty
\end{code}
This are all the packages that we have to import, meaning we can now start to
define the actual model.

\subsection{The Model}

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

-- | The state maps for all the various agents.
s :: Map.Map Atomic (StateMap (Int, Int, Int))
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
hex :: StaticModel (Int, Int, Int)
hex = StaticModel ws v s
\end{code}
