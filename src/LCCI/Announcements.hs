{-| A module that holds examples for update models for different communication
- types
-}
module LCCI.Announcements where
import qualified Data.Map as Map
import qualified Data.Set as Set
import LCCI.Issue
import LCCI.Model
import LCCI.Substitution as Substitution
import LCCI.Syntax
import LCCI.Util

p, q, r :: Proposition
p = proposition "P"
q = proposition "Q"
r = proposition "R"

ingroup, outgroup :: Atomic
ingroup = atom "b"
outgroup = atom "n\\b"

e1, e2 :: Event
e1 = Event 1
e2 = Event 2

-- | An infinite list with events, which is useful for zipping with or taking
-- from. Numbering of this list starts at 1.
eventList :: [Event]
eventList = map Event [1..]

prtStr :: String -> [Formula] -> String
prtStr "" [f] = '!' : prettyShow f
prtStr "" fs  = '!' : prettyShow (IOr fs)
prtStr s  _   = s

-- | Create an issue where the agent entertains which specific event occured
worldIssue :: (Ord a) => [a] -> Issue a
worldIssue = issue . map (state . (:[]))

-- | A function that create an update model for the public announcement of a
-- question. Can be used for the public announcement of a declarative if only
-- one formula is given. The formula will not check if the formulas given are
-- actually declarative. The `String` is used as the name for the update
-- model, and will be substituted by a default if left empty.
publicRaise :: String -> [Atomic] -> [Formula] -> (String, UpdateModel)
publicRaise s as fs = (prtStr s fs, UpdateModel (Set.fromList $ map fst nfs) sms pre sub)
    where nfs = zip eventList fs
          sms = Map.fromList [(a, sm) | a <- as]
          sm  = Map.fromList [(e, iss) | (e, _) <- nfs]
          iss = worldIssue $ map fst nfs
          pre = Map.fromList nfs
          sub = Map.fromList [(e, Substitution.empty) | (e, _) <- nfs]

-- | A function that creates an update model for the public announcement of a
-- given formula. The `Int` is for the maximum size of the model in which the
-- formula will be announced, and can be ignored if the formula is declarative.
-- It is used to calculate the resolutions of the formula. See `publicRaise` for
-- more control over the asking of questions.
public :: [Atomic] -> Formula -> Int -> (String, UpdateModel)
public as f n = publicRaise ('!' : prettyShow f) as (resolutions n f)

-- | A convience function for calling `publicRaise` without having to supply a
-- string. Will always revert to the default.
defaultPublicRaise :: [Atomic] -> [Formula] -> (String, UpdateModel)
defaultPublicRaise = publicRaise ""

-- | Create an update model for the private announcement of a question. In this
-- case, all the agents are aware that a question is being asked, but they are
-- not all aware what this question is. The first list of `Atomic` agents are
-- the ingroup, the group that knows which question is asked. The second are
-- the outgroup, the group that does not know which question is asked, but
-- wants to know, and the third are the agents that do not care about the
-- question being asked. The rest of the parameters are as in `publicRaise`.
privateRaise :: String -> [Atomic] -> [Atomic] -> [Atomic] -> [Formula] -> (String, UpdateModel)
privateRaise s ing outg unintr fs = (prettyShow ing ++ s',
                                     UpdateModel (Set.fromList es) sms pre sub)
    where s'     = prtStr s fs
          nfs    = zip eventList fs
          e0     = Event 0
          es     = map fst nfs ++ [e0]
          sms    = Map.fromList ([(a, insm) | a <- ing] ++ [(a, outsm) | a <- outg] ++
                                 [(a, unsm) | a <- unintr])
          insm   = Map.fromList ((e0, issue [state[e0]]) : [(e, iniss) | (e, _) <- nfs])
          iniss  = worldIssue $ map fst nfs
          outsm  = Map.fromList [(e, outiss) | e <- es]
          outiss = worldIssue es
          unsm   = Map.fromList [(e, uniss) | e <- es]
          uniss  = issue [state es]
          pre    = Map.fromList ((e0, Top) : nfs)
          sub    = Map.fromList [(e, Substitution.empty) | e <- es]

-- | A function that creates an update model for the private announcement of a
-- given formula, where the rest of the group knows that some formula is
-- annouced. See `privateRaise` for explanations on the groups, and `public`
-- for an explanation of the `Formula` and `Int`
private :: [Atomic] -> [Atomic] -> [Atomic] -> Formula -> Int -> (String, UpdateModel)
private i o u f n = privateRaise ('!' : prettyShow f) i o u (resolutions n f)

-- | Create an update model for the secret announcement of a question. In this
-- case, only a subset of the agents (the first parameter) are aware that a
-- question is being asked, while the rest of the agents are not even aware
-- that something is happening. the rest of the parameters are as in
-- `publicRaise`.
secretRaise :: String -> [Atomic] -> [Atomic] -> [Formula] -> (String, UpdateModel)
secretRaise s ing outg fs = (prettyShow ing ++ 's' : prtStr s fs,
                             UpdateModel (Set.fromList es) sms pre sub)
    where nfs = zip eventList fs
          e0  = Event 0
          es  = e0 : map fst nfs
          sms = Map.fromList ([(a, insm) | a <- ing] ++ [(a, outsm) | a <- outg])
          insm = Map.fromList ((e0, issue [state [e0]]) : [(e, iniss) | (e, _) <- nfs])
          iniss = worldIssue $ map fst nfs
          outsm = Map.fromList [(e, issue [state [e0]])| e <- es]
          pre   = Map.fromList ((e0, Top) : nfs)
          sub   = Map.fromList [(e, Substitution.empty) | e <- es]

-- | A function that creates an update model for the secret announcement of a
-- given formula, where the rest of the group is not aware that an announcement
-- has been made. See `secretRaise` for a description of the first two
-- parameters, and `public` for the description of the second two.
secret :: [Atomic] -> [Atomic] -> Formula -> Int -> (String, UpdateModel)
secret i o f n = secretRaise ('!' : prettyShow f) i o (resolutions n f)
