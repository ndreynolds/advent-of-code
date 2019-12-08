#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p haskellPackages.ghc

module Main where

parseInput :: String -> [Int]
parseInput = map read . lines

calcFuel :: Int -> Int
calcFuel x = max ((x `div` 3) - 2) 0

calcFuelRec :: Int -> Int
calcFuelRec x
  | x > 0     = calcFuel x + calcFuelRec (calcFuel x)
  | otherwise = 0

part1 :: [Int] -> Int
part1 = sum . map calcFuel

part2 :: [Int] -> Int
part2 = sum . map calcFuelRec

main :: IO ()
main = interact (\input -> show ( part1 $ parseInput input
                                , part2 $ parseInput input ))
