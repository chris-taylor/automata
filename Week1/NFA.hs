-- |Attempt at writing nondeterministic finite automata in Haskell. I'll use the set monad
--  to handle the nondeterminism, as it is more efficient than the list monad.
module NFA where

import Data.Set (Set)
import qualified Data.Set as Set

import DFA (DFA(DFA))
import qualified DFA as DFA

setOr :: Set Bool -> Bool
setOr = Set.foldr (||) False

setAny :: Ord a => (a -> Bool) -> Set a -> Bool
setAny p = setOr . Set.map p

setConcat :: Ord a => Set (Set a) -> Set a
setConcat = Set.foldr Set.union Set.empty

setBind :: (Ord a, Ord b) => Set a -> (a -> Set b) -> Set b
setBind s f = setConcat $ Set.map f s

-- |Definition of a non-deterministic finite automata.
data NFA s t = NFA
    { delta   :: s -> t -> Set s
    , start   :: s
    , isFinal :: s -> Bool
    }

-- |Run an NFA on an input string, giving a set of possible final states.
run :: Ord s => NFA s t -> [t] -> Set s
run (NFA delta s0 _) = go (Set.singleton s0)
  where
    go s0 []     = s0
    go s0 (t:ts) = s0 `setBind` (\s -> go (delta s t) ts)

-- |Does an NFA accept an input string?
isAccepted :: Ord s => NFA s t -> [t] -> Bool
isAccepted nfa ts = setAny (isFinal nfa) (run nfa ts)

-- |Generate all possible strings of length 'n' from a language.
allWords :: Int -> [a] -> [[a]]
allWords 0 alphabet = [[]]
allWords n alphabet = [ x:xs | xs <- allWords (n-1) alphabet, x <- alphabet ]

-- |Generate all possible strings from a language. This is simply the 'powerset' operator.
powerSet :: [a] -> [[a]]
powerSet alphabet = concat (map (\n -> allWords n alphabet) [0..])

-- |Generate the language associated to a particular NFA for a given alphabet.
language :: Ord s => NFA s t -> [t] -> [[t]]
language nfa alpha = filter (isAccepted nfa) (powerSet alpha)

-- |Generate the strings that are *not* accepted by a particular NFA.
languageC :: Ord s => NFA s t -> [t] -> [[t]]
languageC nfa alpha = filter (not . isAccepted nfa) (powerSet alpha)

------ Convert to DFA --------

-- |Convert an NFA to an equivalent DFA using the subset construction.
nfa2dfa :: Ord s => NFA s t -> DFA (Set s) t
nfa2dfa (NFA delta s0 isFinal) =
    DFA delta' (Set.singleton s0) isFinal'
  where
    delta' set t = set `setBind` \s -> delta s t
    isFinal' set = setAny isFinal set

-- |Build the set of states necessary to represent an NFA as a DFA.
lazyBuildStates :: (Ord s) => NFA s t -> [t] -> [Set s]
lazyBuildStates (NFA delta s0 _) ts = go (Set.singleton s0') [s0']
    where
        s0' = Set.singleton s0

        go seen []     = Set.toList seen
        go seen (s:ss) =
            let move t     = s `setBind` \x -> delta x t
                newStates  = map move ts
                newStates' = filter (\s -> not (Set.member s seen)) newStates
                seen'      = Set.union (Set.fromList newStates) seen
            in go seen' (ss ++ newStates')


------ Examples --------

-- |Chessboard example. An 'R' transition allows a move to any neighboring red square,
--  a 'B' transition allows a move to any neighboring black square. The numbering scheme
--  is
--
--  +---+---+---+
--  | 1 | 2 | 3 |
--  +---+---+---+
--  | 4 | 5 | 6 |
--  +---+---+---+
--  | 7 | 8 | 9 |
--  +---+---+---+
--
-- and '1' is a black square, so an 'R' transition from 1 allows a move to 2 or 4 (which
-- are both red) and a 'B' transition allows a move to 5 (which is black).

ctf 1 'R' = Set.fromList [2,4]
ctf 1 'B' = Set.fromList [5]
ctf 2 'R' = Set.fromList [4,6]
ctf 2 'B' = Set.fromList [1,3,5]
ctf 3 'R' = Set.fromList [2,6]
ctf 3 'B' = Set.fromList [5]
ctf 4 'R' = Set.fromList [2,8]
ctf 4 'B' = Set.fromList [1,5,7]
ctf 5 'R' = Set.fromList [2,4,6,8]
ctf 5 'B' = Set.fromList [1,3,7,9]
ctf 6 'R' = Set.fromList [2,8]
ctf 6 'B' = Set.fromList [3,5,9]
ctf 7 'R' = Set.fromList [4,8]
ctf 7 'B' = Set.fromList [5]
ctf 8 'R' = Set.fromList [4,6]
ctf 8 'B' = Set.fromList [5,7,9]
ctf 9 'R' = Set.fromList [6,8]
ctf 9 'B' = Set.fromList [5]

ctf_isFinal = (==9)

chessboard = NFA ctf 1 ctf_isFinal