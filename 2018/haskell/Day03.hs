-- Day 3: No Matter How You Slice It --
--
-- Usage: runhaskell Day03.hs < ../inputs/03.txt

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Text.Regex.TDFA

data Claim = Claim Integer (Integer, Integer) (Integer, Integer) deriving (Show, Eq)
type Sheet = Map (Integer, Integer) Integer

claimRe :: String
claimRe = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"

parseClaim :: String -> Claim
parseClaim str = Claim id (x, y) (w, h)
 where
  (_, _, _, matches) = str =~ claimRe :: (String, String, String, [String])
  [id, x, y, w, h]   = map read matches

putClaim :: Sheet -> Claim -> Sheet
putClaim sheet (Claim _ (x, y) (w, h)) = Map.unionWith (+) sheet sheetClaims
 where
  sheetClaims =
    Map.fromList [ ((i, j), 1) | i <- [x .. x + w - 1], j <- [y .. y + h - 1] ]

hasIntersection :: Claim -> Claim -> Bool
hasIntersection (Claim _ (ax1, ay1) (aw, ah)) (Claim _ (bx1, by1) (bw, bh)) =
  ax1 < (bx1 + bw) && (ax1 + aw) > bx1 && ay1 < (by1 + bh) && (ay1 + ah) > by1

anyIntersections :: [Claim] -> Claim -> Bool
anyIntersections claims claim =
  any (\c -> c /= claim && hasIntersection claim c) claims

part1 :: [Claim] -> String
part1 claims = show $ length $ filter (> 1) claimCounts
 where
  claimCounts     = Map.elems sheetWithClaims
  sheetWithClaims = foldl putClaim Map.empty claims

part2 :: [Claim] -> String
part2 claims = show $ filter (not . anyIntersections claims) claims

main :: IO ()
main = interact
  (\input -> unlines [part1 (parseClaims input), part2 (parseClaims input)])
  where parseClaims = map parseClaim . lines
