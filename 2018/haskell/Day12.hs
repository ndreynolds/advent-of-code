-- Day 12: Subterranean Sustainability --
--
-- There's no parser for this one, my input is already included.
--
-- Usage: runhaskell Day12.hs

import Graphics.EasyPlot

transform :: String -> String
transform ('#' : '#' : '#' : '#' : ".") = "####."
transform ('#' : '#' : '.' : '#' : ".") = "####."
transform ('#' : '#' : '.' : '.' : ".") = "###.."
transform ('#' : '.' : '#' : '#' : "#") = "#.###"
transform ('#' : '.' : '.' : '#' : ".") = "#.##."
transform ('.' : '#' : '#' : '#' : "#") = ".####"
transform ('.' : '#' : '#' : '.' : "#") = ".##.#"
transform ('.' : '#' : '.' : '.' : "#") = ".##.#"
transform ('.' : '#' : '.' : '.' : ".") = ".##.."
transform ('.' : '.' : '#' : '#' : "#") = "..###"
transform ('.' : '.' : '#' : '.' : "#") = "..#.#"
transform ('.' : '.' : '.' : '#' : "#") = "..###"
transform (l2  : l1  :  _  : r1  : r2 ) = l2 : l1 : '.' : r1 : r2

initial :: String
initial = "######....##.###.#..#####...#.#.....#..#.#.##......###.#..##..#..##..#.##..#####.#.......#.....##.."

generation' :: String -> String
generation' (l2 : l1 : c : r1 : r2 : xs) =
  c' : generation' (l1 : c : r1 : r2 : xs)
  where
    [_, _, c', _, _] = transform [l2, l1, c, r1, r2]
generation' [l1, c, r1, r2] = [r1, r2]

generation :: String -> String
generation str@(l2 : l1 : _) = l2 : l1 : generation' str

evolvedSum :: String -> Int -> Int
evolvedSum start generations = sum $ map fst plants
  where
    plants = filter ((== '#') . snd) $ zip [-padding..] evolved
    evolved = foldl (\acc _ -> generation acc) padded [1..generations]
    padded = pad start

padding :: Int
padding = 100

pad :: String -> String
pad str = replicate padding '.' ++ str ++ replicate padding '.'

part1 :: Int
part1 = evolvedSum initial 20

part2 :: [(Int, Int)]
part2 = zip [1..] $ map (evolvedSum initial) [1..50]

main :: IO Bool
main = do
  print part1
  plot X11 $ Data2D [] [] $ map (\(a, b) -> (fromIntegral a, fromIntegral b)) part2
