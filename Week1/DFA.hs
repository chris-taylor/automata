-- |First attempt at coding up a DFA type in Haskell.
module DFA where

import Data.List (nub)

-- |The DFA type. The type variable 's' represents the state type, and 't' represents the transition type.
data DFA s t = DFA
    { transitionFunction :: s -> t -> s
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
data Ing_State = Start_Ing
			   | Saw_I
			   | Saw_IN
			   | Saw_ING
			   deriving (Show,Eq)

transition_Ing Start_Ing c = case c of
	'i' -> Saw_I
	_   -> Start_Ing

transition_Ing Saw_I     c = case c of
	'i' -> Saw_I
	'n' -> Saw_IN
	_   -> Start_Ing

transition_Ing Saw_IN    c = case c of
	'i' -> Saw_I
	'g' -> Saw_ING
	_   -> Start_Ing

transition_Ing Saw_ING   c = case c of
	'i' -> Saw_I
	_   -> Start_Ing

dfa_ing = DFA {
	transitionFunction = transition_Ing,
	startState = Start_Ing,
	isFinal = (== Saw_ING)
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

