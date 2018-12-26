-- Day 14: Chocolate Charts
--
-- Usage:
--
--   ghc -O2 Day14.hs
--   ./Day14.hs <input>

import           Control.Monad                  ( liftM2 )
import           Data.Sequence                  ( Seq(Empty, (:|>))
                                                , (><)
                                                , (|>)
                                                )
import qualified Data.Sequence                 as S
import           Prelude                 hiding ( seq )
import           System.Environment             ( getArgs )

type Score = Int
type Scoreboard = (Seq Score, Int, Int)

digits :: Integral x => x -> Seq x
digits 0 = S.fromList [0]
digits n = digits' n
 where
  digits' 0 = Empty
  digits' x = digits' (x `div` 10) |> x `mod` 10

rightMatches :: Eq a => Seq a -> Seq a -> Bool
rightMatches Empty _     = True
rightMatches _     Empty = False
rightMatches (xs :|> a) (ys :|> b) | a /= b    = False
                                   | otherwise = rightMatches xs ys

rightMatchesOffset :: (Eq a) => Seq a -> Seq a -> Bool
rightMatchesOffset patt (ys :|> _) = rightMatches patt ys
rightMatchesOffset patt seq        = rightMatches patt seq

generateRecipes :: Scoreboard -> (Seq Score -> Bool) -> Scoreboard
generateRecipes (scores, elf1, elf2) haltTest
  | haltTest scores' = (scores', elf1', elf2')
  | otherwise        = generateRecipes (scores', elf1', elf2') haltTest
 where
  elf1'   = mod (elf1 + recipe1 + 1) (length scores')
  elf2'   = mod (elf2 + recipe2 + 1) (length scores')
  scores' = scores >< digits (recipe1 + recipe2)
  recipe1 = S.index scores elf1
  recipe2 = S.index scores elf2

initialScoreboard :: Scoreboard
initialScoreboard = (S.fromList [3, 7], 0, 1)

part1 :: Int -> Seq Score
part1 x = S.take 10 $ S.drop x scores
 where
  (scores, _, _) = generateRecipes initialScoreboard countReached
  countReached seq = length seq > (x + 10)

part2 :: Int -> Int
part2 x = length scores - length patt
 where
  (scores, _, _) = generateRecipes initialScoreboard matches
  matches        = liftM2 (||) (rightMatches patt) (rightMatchesOffset patt)
  patt           = digits x

main :: IO ()
main = do
  [input] <- fmap (map read) getArgs
  print $ part1 input
  print $ part2 input
