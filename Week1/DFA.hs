-- |First attempt at coding up a DFA type in Haskell.
module DFA where

import Data.List (nub)

-- |The DFA type. The type variable 's' represents the state type, and 't' represents the transition type.
data DFA s t = DFA
    { delta :: s -> t -> s
    , startState :: s
    , isFinal :: s -> Bool }

-- |Consume a string with a given DFA, outputting the final state.
run :: DFA s t -> [t] -> s
run (DFA delta s0 _) = go s0
  where
    go s []     = s
    go s (t:ts) = go (delta s t) ts

-- |Does a given DFA accept a string?
isAccepted :: DFA s t -> [t] -> Bool
isAccepted dfa ts = isFinal dfa (run dfa ts)

-- |Generate all possible strings of length 'n' from a language.
allWords :: Int -> [a] -> [[a]]
allWords 0 alphabet = [[]]
allWords n alphabet = [ x:xs | xs <- allWords (n-1) alphabet, x <- alphabet ]

-- |Generate all possible strings from a language. This is simply the 'powerset' operator.
powerSet :: [a] -> [[a]]
powerSet alphabet = concat (map (\n -> allWords n alphabet) [0..])

-- |Generate the language associated to a particular DFA for a given alphabet.
language :: DFA s t -> [t] -> [[t]]
language dfa alpha = filter (isAccepted dfa) (powerSet alpha)

-- |Generate a list of all the observable states for a given set of inputs.
observableStates :: Eq s => DFA s t -> [[t]] -> [s]
observableStates dfa = nub . map (run dfa)

--------------
-- Examples --
--------------

-- |DFA that recognizes a string ending in "ing". The alphabet is the set of all lists of ASCII characters.
data Ing_State = Saw String deriving (Show,Eq)

transition_Ing (Saw "")    c = case c of
    'i' -> Saw "i"
    _   -> Saw ""

transition_Ing (Saw "i")   c = case c of
    'i' -> Saw "i"
    'n' -> Saw "in"
    _   -> Saw ""

transition_Ing (Saw "in")  c = case c of
    'i' -> Saw ""
    'g' -> Saw "ing"
    _   -> Saw ""

transition_Ing (Saw "ing") c = case c of
    'i' -> Saw "i"
    _   -> Saw ""

dfa_ing = DFA {
    delta = transition_Ing,
    startState = Saw "",
    isFinal = (== Saw "ing")
}

-- |DFA that recognizes strings that contain no consecutive 1s.
data State_11 = A | B | C deriving (Eq,Show)

transition_11 A '0' = A
transition_11 A '1' = B
transition_11 B '0' = A
transition_11 B '1' = C
transition_11 C  _  = C

dfa_11 = DFA transition_11 A (\s -> s==A || s==B)

-- |DFA that recognizes whether a given string of 0s and 1s, when viewed as a
--  binary integer, is divisible by 23/
data State_23 = State_23 Int deriving (Show)

mk23 i | i >= 0 && i < 23 = State_23 i
       | otherwise        = error "i not in [0,23)"

transition_23 (State_23 i) '0' = mk23 $ rem (2 * i)     23
transition_23 (State_23 i) '1' = mk23 $ rem (2 * i + 1) 23

dfa_23 = DFA transition_23 (mk23 0) (\(State_23 i) -> i == 0)

-------------------------
-- Word-matching DFA
-------------------------

-- |States for the word-matching DFA.
data WordState = Seen Int
               | Fail
               deriving (Eq, Ord, Show)

-- |DFA to match a fixed word.
dfa_word :: String -> DFA WordState Char
dfa_word str = DFA delta s0 isFinal
  where
    delta = go
    s0    = Seen 0
    isFinal  Fail    = False
    isFinal (Seen n) = n == length str

    go  Fail    _ = Fail
    go (Seen n) c = if n == length str
        then Fail
        else if c /= head (drop n str)
            then Fail
            else Seen (n+1)


