-- Day 9: Marble Mania --
--
-- Usage: runhaskell Day09.hs <num-players> <last-marble>
--
-- Example: runhaskell Day09.hs 464 71730

import System.Environment (getArgs)

type Marble = Integer
type Circle = [Integer]
type PlayerId = Integer
type Scores = [(PlayerId, Integer)]

type Game = (Circle, Scores)

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop n (cycle xs)

twentyThird :: Game -> Marble -> Game
twentyThird (circle, ((player, score) : otherPlayers)) m =
  (newCircle, rotate 1 ((player, newScore) : otherPlayers))
  where
    newScore = score + m + removed
    newCircle = rotate 6 (hd ++ tl)
    (hd, removed : tl) = splitAt 7 circle

putMarble :: Game -> Marble -> Game
putMarble ([0],    scores) 1 = ([1, 0], rotate 1 scores)
putMarble ([1, 0], scores) 2 = ([2, 0, 1], rotate 1 scores)
putMarble (xs, scores) m
  | m `rem` 23 == 0 = twentyThird (xs, scores) m
  | otherwise       = (m : rotate (pred $ length xs) xs, rotate 1 scores)

part1 :: Integer -> Integer -> Integer
part1 numPlayers lastMarble = maximum $ map snd scores
  where
    (_circle, scores) = foldl putMarble ([0], players) marbles
    players = [(i, 0) | i <- [1..numPlayers]]
    marbles = [1..lastMarble]

main :: IO ()
main = do
  [numPlayers, lastMarble] <- fmap (map read) getArgs
  print $ part1 numPlayers lastMarble
