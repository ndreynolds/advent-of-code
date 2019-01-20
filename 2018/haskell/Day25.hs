-- Day 25: Four-Dimensional Adventure --
--
-- Usage: runhaskell Day25.hs < ../inputs/25.txt

import           Data.Graph                     ( graphFromEdges
                                                , components
                                                )
import           Data.List.Split                ( splitOn )

type Coord = (Int, Int, Int, Int)

manhattanDist :: Coord -> Coord -> Int
manhattanDist (w, x, y, z) (w', x', y', z') =
  abs (w - w') + abs (x - x') + abs (y - y') + abs (z - z')

parseCoord :: String -> Coord
parseCoord input = (read w, read x, read y, read z)
  where [w, x, y, z] = splitOn "," input

part1 :: [Coord] -> Int
part1 coords = length $ components graph
  where
    (graph, _, _) = graphFromEdges [ edge x | x <- coords ]
    edge x = (x, x, [ y | y <- coords, manhattanDist x y <= 3 ])

main :: IO ()
main = interact (show . part1 . map parseCoord . lines)
