-- Day 11: Chronal Charge --
--
-- Requires matrix package. The part 2 solution is currently unoptimized. It
-- takes a few minutes to complete. Could be optimizied with a summed area
-- table.
--
-- Usage:
--   ghc -O3 Day11.hs
--   ./Day11 <serial>
--
-- Example:
--   ./Day11 6303

import Data.List (maximumBy)
import Data.Matrix (Matrix, fromList, submatrix)
import Data.Ord (comparing)
import System.Environment (getArgs)

type CoordWithSize = (Int, Int, Int)

powerLevel :: Int -> Int -> Int -> Int
powerLevel serial x y =
  hundredth ((rackId * y + serial) * rackId) - 5
  where
    hundredth n = (n `div` 100) `mod` 10
    rackId = x + 10

width :: Int
width = 300

height :: Int
height = 300

grid :: Int -> Matrix Int
grid serial = fromList width height [powerLevel serial x y | x <- [1..width], y <- [1..height]]

square :: Int -> Int -> Int -> Matrix Int -> Matrix Int
square size x y = submatrix x (x + size - 1) y (y + size - 1)

submatrices :: Int -> (Int, Int) -> Matrix Int -> [(CoordWithSize, Matrix Int)]
submatrices size (x0, y0) matrix =
  [((x, y, size), square size x y matrix) | x <- [x0..xMax], y <- [y0..yMax]]
  where
    xMax = width - pred size
    yMax = height - pred size

part2 :: Int -> CoordWithSize
part2 serial = fst mostPower
  where
    mostPower = maximumBy (comparing (sum . snd)) allSubs
    allSubs = concat [submatrices size (1, 1) (grid serial) | size <- [3..100]]

part1 :: Int -> CoordWithSize
part1 serial = fst mostPower
  where
    mostPower = maximumBy (comparing (sum . snd)) $ submatrices 3 (1, 1) (grid serial)

main :: IO ()
main = do
  [serial] <- fmap (map read) getArgs
  print $ part1 serial
  print $ part2 serial
