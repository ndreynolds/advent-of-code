-- Day 10: The Stars Align --
--
-- Requires the easyplot package and the gnuplot executable in $PATH.
--
-- The algorithm looks for a high frequency of vertically and horizontally
-- aligned points, then selects and plots the best candidate.
--
-- Usage:
--
--   runhaskell Day10.hs < ../inputs/10.txt
--
-- Then to make the letters readable:
--
--   gnuplot -e 'set term x11 persist; set size 0.5,0.15; plot "plot1.dat"'

import Data.List (maximumBy)
import Data.Map (elems, fromListWith)
import Data.Ord (comparing)
import Graphics.EasyPlot
import Text.Regex.TDFA

type Pair = (Double, Double)

positionRe :: String
positionRe = "position=<[[:space:]]*(-?[0-9]+),[[:space:]]*(-?[0-9]+)>"

velocityRe :: String
velocityRe = "velocity=<[[:space:]]*(-?[0-9]+),[[:space:]]*(-?[0-9]+)>"

captures :: String -> String -> [String]
captures str re = xs
  where
    (_, _, _, xs) = str =~ re :: (String, String, String, [String])

parsePoint :: String -> (Pair, Pair)
parsePoint str = ((px, py), (vx, vy))
 where
  [px, py] = map read $ captures str positionRe
  [vx, vy] = map read $ captures str velocityRe

transform :: (Pair, Pair) -> (Pair, Pair)
transform ((px, py), (vx, vy)) = ((px + vx, py + vy), (vx, vy))

frequencies :: Ord a => [a] -> [Int]
frequencies xs = elems $ fromListWith (+) [ (x, 1) | x <- xs ]

-- Some manual tweaking was required to arrive at 30 :)
countAligned :: Ord a => [a] -> Int
countAligned = sum . filter (> 30) . frequencies

countVertAligned :: [Pair] -> Int
countVertAligned = countAligned . map snd

countHorizAligned :: [Pair] -> Int
countHorizAligned = countAligned . map fst

search :: Integer -> [(Int, [Pair])] -> [(Pair, Pair)] -> [(Int, [Pair])]
search 0 acc _ = acc
search iterations acc points
  | x > 0     = search (pred iterations) ((x, positions) : acc) newPoints
  | otherwise = search (pred iterations) acc newPoints
  where
    x = countVertAligned positions + countHorizAligned positions
    positions = map fst newPoints
    newPoints = map transform points

main :: IO Bool
main = do
  input <- getContents
  plot X11 $ Data2D [] [Step 1] $ bestCandidate $ parse input
  where
    bestCandidate = snd . maximumBy (comparing fst) . search 15000 []
    parse = map parsePoint . lines