-- |Attempt at writing an epsilon-NFA type.
module EpsilonNFA where

import Data.Set (Set, member)
import qualified Data.Set as Set

setOr :: Set Bool -> Bool
setOr = Set.foldr (&&) False

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
    -- go s0 []     = setClosure nfa s0
    -- go s0 (t:ts) = do
    --     s  <- s0              // pick initial element
    --     s' <- closure nfa s   // follow epsilon transitions
    --     go (delta s t) ts     // continue







-- Might be a useful utility function?

getClosure :: (Ord s) => (s -> Set s) -> Set s -> Set s
getClosure f initial = go initial initial
  where
    go closed open
        | Set.null open = closed
        | otherwise     = 
            let (a, old) = Set.deleteFindMin open
                new      = Set.filter (not . (`member` closed)) (f a)
            in go (Set.union closed new) (Set.union old new)


