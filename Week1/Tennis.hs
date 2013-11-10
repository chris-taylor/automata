module Tennis where

{-

Pros: works correctly.

Cons: impossible scorelines are representable in the type system, leading to a complicated definition of 'nextScore'. For example, 'ScoreLine Forty Forty' is a possible result, so we end up having to write a special case for 'ScoreLine Thirty Forty' and 'ScoreLine Forty Thirty'.

It's not immediately obvious that all cases are covered in 'nextScore'.

-}

data Player = A | B deriving (Eq,Show)

data Score = Love | Fifteen | Thirty | Forty deriving (Show)

inc Love    = Fifteen
inc Fifteen = Thirty
inc Thirty  = Forty
inc Forty   = error "No score beyond forty"

type Game = [Player]

data ScoreLine = Game Player
               | Advantage Player
               | Deuce
               | ScoreLine Score Score
               deriving (Show)

nextScore (Game _)                 _ = error "Game already won"
nextScore  Deuce                   p = Advantage p
nextScore (Advantage q)            p = if q == p then Game q else Deuce
nextScore (ScoreLine Thirty Forty) p = if p == A then Deuce else Game B
nextScore (ScoreLine Forty Thirty) p = if p == B then Deuce else Game A
nextScore (ScoreLine a Forty)      p = if p == A then (ScoreLine (inc a) Forty) else Game B
nextScore (ScoreLine Forty b)      p = if p == B then (ScoreLine Forty (inc b)) else Game A
nextScore (ScoreLine a b)          A = ScoreLine (inc a) b
nextScore (ScoreLine a b)          B = ScoreLine a (inc b)

initialScore = ScoreLine Love Love

scoreGame = foldr (flip nextScore) initialScore