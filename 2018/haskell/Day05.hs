-- Day 5: Alchemical Reduction --
--
-- Usage: runhaskell Day05.hs < ../inputs/05.txt

import Data.Char
import Data.List

react :: (Integer, String) -> String -> (Integer, String)
react (deletes, acc) (a : b : rest)
  | a /= b && toLower a == toLower b = react (succ deletes, acc) rest
  | otherwise                        = react (deletes, a : acc) (b : rest)
react (deletes, acc) [a] = react (deletes, a : acc) []
react (deletes, acc) [] = (deletes, reverse acc)

reactFully :: String -> String
reactFully str =
  case (react (0, "") str) of
    (0, remainder) -> remainder
    (_, remainder) -> reactFully remainder

uniqueChars :: String -> String
uniqueChars str = nub $ map toLower str

part1 :: String -> String
part1 input = show $ length $ reactFully input

part2 :: String -> String
part2 input = show $ minimum $ map (length . reactFully) versions
  where
    versions = map (\c -> filter ((/= c) . toLower) input) uniques
    uniques = uniqueChars input

main :: IO ()
main = interact (\input -> unlines [part1 $ parse input, part2 $ parse input])
  where
    parse = head . lines
