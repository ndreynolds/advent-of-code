-- Day 2: Inventory Management System --
--
-- Usage: runhaskell Day02.hs < ../inputs/02.txt

import Data.Map (fromListWith, elems)

frequencies :: Ord a => [a] -> [Int]
frequencies xs = elems $ fromListWith (+) [ (x, 1) | x <- xs ]

hasDuplicate :: Ord a => [a] -> Bool
hasDuplicate = elem 2 . frequencies

hasTriplicate :: Ord a => [a] -> Bool
hasTriplicate = elem 3 . frequencies

isCloseMatch :: String -> String -> Bool
isCloseMatch x y = sum [ if a == b then 0 else 1 | (a, b) <- zip x y ] <= 1

closeMatches :: [String] -> [(String, String)]
closeMatches xs = [ (a, b) | a <- xs, b <- xs, a /= b, isCloseMatch a b ]

commonChars :: (String, String) -> String
commonChars (x, y) = [ a | (a, b) <- zip x y, a == b ]

part1 :: [String] -> String
part1 ids = show $ numTwice * numThrice
 where
  numTwice  = length $ filter hasDuplicate ids
  numThrice = length $ filter hasTriplicate ids

part2 :: [String] -> String
part2 = show . commonChars . head . closeMatches

main :: IO ()
main =
  interact (\input -> unlines [part1 (lines input), part2 (lines input)])
