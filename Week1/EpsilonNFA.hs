-- |Attempt at writing an epsilon-NFA type.
module EpsilonNFA where

import Data.Set (Set, member)
import qualified Data.Set as Set
import Data.IntMap ((!))
import qualified Data.IntMap as IM
import Control.Monad (filterM)


import qualified DFA as DFA
import qualified NFA as NFA

setOr :: Set Bool -> Bool
setOr = Set.foldr (||) False

setAny :: Ord a => (a -> Bool) -> Set a -> Bool
setAny p = setOr . Set.map p

setConcat :: Ord a => Set (Set a) -> Set a
setConcat = Set.foldr Set.union Set.empty

setBind :: (Ord a, Ord b) => Set a -> (a -> Set b) -> Set b
setBind s f = setConcat $ Set.map f s

(>>-) :: (Ord a, Ord b) => Set a -> (a -> Set b) -> Set b
(>>-) = setBind

----- ENFA type -----

data Transition s = Epsilon | T s deriving (Eq,Ord,Show)

data EpsNFA s t = EpsNFA
    { delta   :: s -> Transition t -> Set s
    , start   :: s
    , isFinal :: s -> Bool
    }

closure :: (Ord s) => EpsNFA s t -> s -> Set s
closure (EpsNFA delta _ _) s0 = go (Set.singleton s0) [s0]
  where
    go seen []     = seen
    go seen (s:ss) =
        let new   = delta s Epsilon
            new'  = Set.filter (not . (`member` seen)) new
            seen' = Set.union seen new'
        in go seen' (ss ++ Set.toList new')

setClosure :: (Ord s) => EpsNFA s t -> Set s -> Set s
setClosure nfa s = s >>- closure nfa

run :: (Ord s) => EpsNFA s t -> [t] -> Set s
run nfa@(EpsNFA delta s0 _) = go (Set.singleton s0) . map T
  where
    cl = setClosure nfa

    go s0 []     = cl s0
    go s0 (t:ts) = cl s0 >>- \s -> go (delta s t) ts

    -- /* If 'Set a' was a Monad than we could use
    --    do notation as follows: */
    --
    -- go s0 []     = do
    --     s <- s0               // pick initial element
    --     closure nfa s         // epsilon transition to final states
    -- go s0 (t:ts) = do
    --     s  <- s0              // pick initial element
    --     s' <- closure nfa s   // follow epsilon transitions
    --     go (delta s' t) ts    // continue

-- |Is a string accepted by this epsilon-NFA?
isAccepted :: (Ord s) => EpsNFA s t -> [t] -> Bool
isAccepted nfa str = setAny (isFinal nfa) (run nfa str)

-- |Generate all possible strings of length 'n' from a language.
allWords :: Int -> [a] -> [[a]]
allWords 0 alphabet = [[]]
allWords n alphabet = [ x:xs | xs <- allWords (n-1) alphabet, x <- alphabet ]

-- |Generate all possible strings from a language. This is simply the 'powerset' operator.
powerSet :: [a] -> [[a]]
powerSet alphabet = concat (map (\n -> allWords n alphabet) [0..])

-- |Generate the language associated to a particular NFA for a given alphabet.
language :: Ord s => EpsNFA s t -> [t] -> [[t]]
language nfa alpha = filter (isAccepted nfa) (powerSet alpha)

-- |Generate the strings that are *not* accepted by a particular NFA.
languageC :: Ord s => EpsNFA s t -> [t] -> [[t]]
languageC nfa alpha = filter (not . isAccepted nfa) (powerSet alpha)

-- |Transform an epsilon-NFA into an NFA without epsilon transitions.
enfa2nfa :: Ord s => EpsNFA s t -> NFA.NFA s t
enfa2nfa enfa@(EpsNFA delta s0 isFinal) =
    NFA.NFA delta' s0 isFinal'
  where
    delta' s t = closure enfa s >>- \x -> delta x (T t)
    isFinal' s = setAny isFinal (closure enfa s)





--------------
-- Examples
--------------

enum = zip [0..]

data WordState_ = Start
                | Word Int DFA.WordState
                deriving (Eq, Ord, Show)

nfa_words :: [ String ] -> EpsNFA WordState_ Char
nfa_words strs = EpsNFA f s0 isFinal
  where

    f  Start Epsilon = Set.fromList $ map (\(i,s) -> Word i (DFA.Seen 0)) (enum strs)
    f  Start _       = Set.empty

    f (Word _ _ ) Epsilon = Set.empty
    f (Word i ws) (T t)   = Set.singleton $ Word i (DFA.delta (dfas!i) ws t)

    s0 = Start
    
    isFinal  Start      = False
    isFinal (Word i ws) = DFA.isFinal (dfas!i) ws

    dfas :: IM.IntMap (DFA.DFA DFA.WordState Char)
    dfas = IM.fromList . enum $ map DFA.dfa_word strs


-- Might be a useful utility function?

-- |Given a function f: S -> 2^S from a set to its power set, this
--  computes the closure of any subset S' < S under repeated
--  application of f to each element. More formally define
--
--    g(S') = \union_{s in S'} f(s)
--
--  then this computes
--
--    Orb_g(S') = { t in S : t in g^k(S') for some k = 0, 1, 2, ... }
--  
--  i.e. the orbit of S' under g. I think this might make a good
--  interview question...
getClosure :: (Ord s) => (s -> Set s) -> Set s -> Set s
getClosure f initial = go initial initial
  where
    go closed open
        | Set.null open = closed
        | otherwise     = 
            let (a, old) = Set.deleteFindMin open
                new      = Set.filter (not . (`member` closed)) (f a)
            in go (Set.union closed new) (Set.union old new)

-- Python version?
-- 
-- def get_closure(f, initial):
--     '''f should be a function x -> set(x)
--        initial should be a list(x) or set(x)'''
--     open, closed = initial, set(initial)
--     while open:
--         x = open.pop()
--         new = f(x)
--         for y in new:
--             if y not in closed:
--                 open.append(y)
--         closed.update(new)
--     return closed

getClosure1 :: (Ord s) => (s -> Set s) -> s -> Set s
getClosure1 f s = getClosure f (Set.singleton s)

getClosure2 :: (Ord s) => (s -> s) -> s -> Set s
getClosure2 f s = getClosure (Set.singleton . f) (Set.singleton s)