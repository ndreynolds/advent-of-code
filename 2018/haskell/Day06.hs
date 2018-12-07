-- Day 6: Chronal Coordinates --
--
-- Usage:
--
--   $ ghc -O Day06.hs
--   $ ./Day06 < ../inputs/06.txt

import Data.List ( minimumBy, maximumBy, groupBy, sortBy )
import Data.Ord ( comparing )

type Coordinates = (Integer, Integer)

manhattanDist :: Coordinates -> Coordinates -> Integer
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

closestNeighbor :: Coordinates -> [Coordinates] -> Maybe Coordinates
closestNeighbor point neighbors
  | (count minDist dists) == 1 = Just minCoords
  | otherwise                  = Nothing
  where
    count x = length . filter ((x ==) . snd)
    (minCoords, minDist) = minimumBy (comparing snd) dists
    dists                = [ (x, manhattanDist point x) | x <- neighbors ]

xMax :: Integer
xMax = 400

yMax :: Integer
yMax = 400

isEdge :: Coordinates -> Bool
isEdge (x, y) = x `elem` [0, xMax] || y `elem` [0, yMax]

part1 :: [Coordinates] -> String
part1 coordPairs = show $ length largest
  where
    largest  = maximumBy (comparing length) filtered
    filtered = filter (not . any (isEdge . fst)) grouped
    grouped =
      groupBy (\a b -> snd a == snd b) $ sortBy (comparing snd) neighborsMap
    neighborsMap =
      [ ((i, j), closestNeighbor (i, j) coordPairs)
      | i <- [0 .. xMax]
      , j <- [0 .. yMax]
      ]

part2 :: [Coordinates] -> String
part2 coordPairs = show $ length filtered
  where
    filtered = filter ((< 10000) . snd) totalDistMap
    totalDistMap =
      [ ((i, j), sum $ map (manhattanDist (i, j)) coordPairs)
      | i <- [0 .. xMax]
      , j <- [0 .. yMax]
      ]

main :: IO ()
main = interact (\input -> unlines [part1 $ parse input, part2 $ parse input])
  where parse input = [ read $ "(" ++ l ++ ")" | l <- lines input ]
