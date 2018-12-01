-- Day 1: Chronal Calibration --
--
-- Usage: runhaskell Day01.hs < inputs/01.txt

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

parse :: String -> Integer
parse ('+' : str) = read str :: Integer
parse str         = read str :: Integer

modulations :: String -> [Integer]
modulations = map parse . lines

duplicateFreq :: Set Integer -> Integer -> [Integer] -> Integer
duplicateFreq prevFreqs freq mods
  | Set.member freq prevFreqs = freq
  | otherwise = duplicateFreq (Set.insert freq prevFreqs)
                              (freq + head mods)
                              (tail mods)

part1 :: String -> String
part1 = show . sum . modulations

part2 :: String -> String
part2 = show . duplicateFreq Set.empty 0 . cycle . modulations

main :: IO ()
main = interact (\input -> unlines [part1 input, part2 input])
