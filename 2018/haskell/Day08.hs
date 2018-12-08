-- Day 8: Memory Maneuver --
--
-- Usage: runhaskell Day08.hs < ../inputs/08.txt

import Data.List.Split (splitOn)
import Data.Tree

type LicenseTree = Tree [Int]

parseN :: Int -> [Int] -> ([LicenseTree], [Int])
parseN 0 input = ([], input)
parseN n input = (node : others, rest)
  where
    (others, rest) = parseN (pred n) rest'
    (node, rest') = parseTree input

parseTree :: [Int] -> (LicenseTree, [Int])
parseTree (childCount : metadataCount : xs) =
  (Node metadata children, drop metadataCount rest)
  where
    metadata = take metadataCount rest
    (children, rest) = parseN childCount xs

value :: LicenseTree -> Int
value (Node metadata [] ) = sum metadata
value (Node metadata children) = sum $ map (value . (children !!)) indices
  where
    indices = filter (< length children) $ map pred metadata

part1 :: LicenseTree -> String
part1 = show . foldTree (\xs acc -> sum (xs ++ acc))

part2 :: LicenseTree -> String
part2 = show . value

main :: IO ()
main = interact (\input -> unlines [part1 $ parse input, part2 $ parse input])
  where
    parse = fst . parseTree . map read . splitOn " " . head . lines
