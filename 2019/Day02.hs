#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.split])"

module Main where

import Data.List.Split (splitOn)
import Data.List       (splitAt, find)

parseInput :: String -> [Int]
parseInput = map read . (splitOn ",")

putAt :: Int -> Int -> [Int] -> [Int]
putAt idx val xs = before ++ [val] ++ after
    where (before, (_ : after)) = splitAt idx xs

eval :: Int -> [Int] -> Int
eval ptr xs =
    case op of
         (99 : _) ->
            xs !! 0
         [1, x, y, r] ->
            eval (ptr + 4) (putAt r (xs !! x + xs !! y) xs)
         [2, x, y, r] ->
            eval (ptr + 4) (putAt r (xs !! x * xs !! y) xs)
         _ ->
            error "invalid sequence"

    where (_, tail) = splitAt ptr xs
          (op, _) = splitAt 4 tail

part1 :: [Int] -> Int
part1 = eval 0

part2 :: [Int] -> Maybe (Int, Int)
part2 (x : _ : _ : xs) = find (\(noun, verb) -> eval 0 (x : noun : verb : xs) == 19690720)
                              [(noun, verb) | noun <- [0..99], verb <- [0..99]]

main :: IO ()
main = interact (\input -> show ( part1 $ parseInput input
                                , part2 $ parseInput input ))
