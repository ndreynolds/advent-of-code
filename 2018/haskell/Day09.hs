-- Day 9: Marble Mania --
--
-- Usage: runhaskell Day09.hs <num-players> <last-marble>
--
-- Examples:
--
--   runhaskell Day09.hs 464 71730

import Prelude hiding (drop, length, take, splitAt)
import Data.Sequence
import System.Environment (getArgs)

type Marble = Integer
type Circle = Seq Integer
type PlayerId = Integer
type Scores = Seq (PlayerId, Integer)

type Game = (Circle, Scores)

rotate :: Int -> Seq a -> Seq a
rotate 0 xs = xs
rotate _ Empty = Empty
rotate n xs = drop n xs >< take n xs

twentyThird :: Game -> Marble -> Game
twentyThird (circle, (player, score) :<| otherPlayers) m =
  (rotate 6 (hd >< tl), otherPlayers |> (player, score + m + removed))
  where
    (hd, removed :<| tl) = splitAt 7 circle

putMarble :: Game -> Marble -> Game
putMarble (xs, scores) m
  | m `rem` 23 == 0 = twentyThird (xs, scores) m
  | otherwise       = (m <| rotate (pred $ length xs) xs, rotate 1 scores)

highestScore :: Integer -> Integer -> Integer
highestScore numPlayers lastMarble = maximum $ fmap snd scores
  where
    (_circle, scores) = foldl putMarble (initial, players) marbles
    initial = fromList [0]
    players = fromList [(i, 0) | i <- [1..numPlayers]]
    marbles = fromList [1..lastMarble]

main :: IO ()
main = do
  [numPlayers, lastMarble] <- fmap (map read) getArgs
  print $ highestScore numPlayers lastMarble
