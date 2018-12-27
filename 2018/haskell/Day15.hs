-- Day 15: Beverage Bandits
--
-- Requires additionally the PSQueue package (psqueue-1.1) for the priority
-- queue implementation.
--
-- Usage: runhaskell Day15.hs < ../inputs/15.txt

import           Data.Function                  ( on )
import           Data.List                      ( intercalate
                                                , minimumBy
                                                , sortBy
                                                , sortOn
                                                )
import           Data.List.Split                ( chunksOf )
import           Data.Map                       ( Map
                                                , (!)
                                                , (!?)
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import           Data.Ord                       ( comparing )
import           Data.PSQueue                   ( Binding(..) )
import qualified Data.PSQueue                  as PQ
import           Prelude                 hiding ( round )
import           Debug.Trace


-- TYPES

data Square = SOpen
            | SWall
            | SUnit Unit Int
            deriving (Show, Eq)

data Unit = Goblin
          | Elf
          deriving (Show, Eq)

type Coord = (Int, Int)

type Path = [Coord]

type BattleMap = Map Coord Square


-- PARSING

parseSquare :: Char -> Square
parseSquare '#' = SWall
parseSquare '.' = SOpen
parseSquare 'G' = SUnit Goblin 200
parseSquare 'E' = SUnit Elf 200
parseSquare x   = error $ "parse error at: " ++ show x

parseRow :: Int -> String -> [(Coord, Square)]
parseRow y row = [ ((x, y), parseSquare ch) | (ch, x) <- zip row [0 ..] ]

parseMap :: [String] -> BattleMap
parseMap rows =
  Map.fromList $ concat [ parseRow y line | (line, y) <- zip rows [0 ..] ]


-- DISPLAY

displaySquare :: Square -> Char
displaySquare SWall            = '#'
displaySquare SOpen            = '.'
displaySquare (SUnit Goblin _) = 'G'
displaySquare (SUnit Elf    _) = 'E'

displayMap :: BattleMap -> String
displayMap battleMap = intercalate "\n" $ chunksOf (xMax + 1) squares
 where
  squares =
    [ displaySquare (battleMap ! (x, y)) | y <- [0 .. yMax], x <- [0 .. xMax] ]
  xMax   = maximum $ map fst coords
  yMax   = maximum $ map snd coords
  coords = Map.keys battleMap


-- PATH-FINDING

manhattanDist :: Coord -> Coord -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Update for larger maps
maxDist :: Int
maxDist = 1000

-- TODO: Tune to prefer reading order
dijkstra :: BattleMap -> Coord -> Coord -> Maybe Path
dijkstra battleMap origin goal = retrace prunedPrev origin goal []
 where
  prunedPrev = prune (PQ.insert origin 0 initQ) initPrev

  initPrev   = Map.empty
  initQ =
    PQ.fromList [ coord :-> maxDist | (coord, SOpen) <- Map.toList battleMap ]

  retrace prev a b path
    | a == b = Just path
    | otherwise = case prev !? b of
      Just b' -> retrace prev a b' (b : path)
      Nothing -> Nothing

  prune q prev | PQ.null q = prev
               | otherwise = prune (PQ.deleteMin q') prev'
   where
    (prev', q') = foldl (update u) (prev, q) neighbors
    neighbors = [ n | n <- openAdjacents battleMap u, isJust (PQ.lookup n q) ]
    Just (u :-> _) = PQ.findMin q

  update u (prev, q') v
    | alt < vCost = (Map.insert v u prev, PQ.adjust (\_ -> alt) v q')
    | otherwise   = (prev, q')
   where
    alt        = uCost + 1
    Just uCost = PQ.lookup u q'
    Just vCost = PQ.lookup v q'


-- UTILITY FUNCTIONS

open :: BattleMap -> Coord -> Bool
open battleMap coord = battleMap !? coord == Just SOpen

isUnit :: BattleMap -> Unit -> Coord -> Bool
isUnit battleMap unit coord = case battleMap !? coord of
  Just (SUnit unit' _) -> unit == unit'
  _                    -> False

enemyOf :: Unit -> Unit
enemyOf Goblin = Elf
enemyOf Elf    = Goblin

adjacents :: Coord -> [Coord]
adjacents (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

openAdjacents :: BattleMap -> Coord -> [Coord]
openAdjacents battleMap = filter (open battleMap) . adjacents

adjacentEnemiesOf :: BattleMap -> Unit -> Coord -> [(Coord, Square)]
adjacentEnemiesOf bm unit coord =
  [ (c, sq)
  | (c, Just sq@(SUnit unit' _)) <- adjCoordsWithSquare
  , unit' == enemyOf unit
  ]
 where
  adjCoordsWithSquare = zip adjCoords $ map (bm !?) adjCoords
  adjCoords           = adjacents coord

allEnemiesOf :: BattleMap -> Unit -> [(Coord, Square)]
allEnemiesOf bm unit =
  [ (coord, sq)
  | (coord, sq@(SUnit unit' _)) <- Map.toList bm
  , unit' == enemyOf unit
  ]

allUnits :: BattleMap -> [(Coord, Square)]
allUnits bm = [ (coord, sq) | (coord, sq@(SUnit _ _)) <- Map.toList bm ]

-- | Coordinate of nearest square adjacent to an enemy with the path there.
nearestEnemyAdjacent :: BattleMap -> Unit -> Coord -> Maybe (Coord, Path)
nearestEnemyAdjacent bm unit coord = case candidates of
  [] -> Nothing
  _  -> Just $ minimumBy (readingOrderCmp `on` fst) candidates
 where
  candidates = filter ((== minDist) . length . snd) coordsWithPath
  minDist    = minimum $ map (length . snd) coordsWithPath
  coordsWithPath =
    [ (coord', path) | (coord', Just path) <- coordsWithMaybePath ]
  coordsWithMaybePath =
    map (\coord' -> (coord', dijkstra bm coord coord')) enemyAdjacents
  enemyAdjacents = concatMap (openAdjacents bm . fst) enemies
  enemies        = allEnemiesOf bm unit

-- | First row from left to right, then second row from left to right, and so on...
readingOrderCmp :: Coord -> Coord -> Ordering
readingOrderCmp (x, y) (x', y') = case compare y y' of
  EQ    -> compare x x'
  other -> other

-- | First by hit points, then by reading order.
attackOrderCmp :: (Coord, Square) -> (Coord, Square) -> Ordering
attackOrderCmp (coord, SUnit _ hp) (coord', SUnit _ hp') =
  case compare hp hp' of
    EQ    -> readingOrderCmp coord coord'
    other -> other
attackOrderCmp _ _ = error "can't compare non-units for attack priority"


-- GAMEPLAY

attackPower :: Int
attackPower = 3

attack :: BattleMap -> (Coord, Square) -> BattleMap
attack bm (coord, SUnit unit hp)
  | hp > 3    = Map.insert coord (SUnit unit (hp - 3)) bm
  | otherwise = Map.insert coord SOpen bm
attack bm _ = bm

move :: BattleMap -> (Coord, Square) -> Coord -> BattleMap
move bm (coord, unit) coord' =
  Map.insert coord' unit (Map.insert coord SOpen bm)

unitRound :: BattleMap -> (Coord, Square) -> BattleMap
unitRound bm sq@(coord, SUnit unit _hp)
  | not (null adjEnemies)  = attack bm (head sortedAdjEnemies)
  | isJust nearestEnemyAdj = move bm sq (firstStepTowards nearestEnemyAdj)
  | otherwise              = bm
 where
  firstStepTowards = head . snd . fromJust
  nearestEnemyAdj  = nearestEnemyAdjacent bm unit coord
  sortedAdjEnemies = sortBy attackOrderCmp adjEnemies
  adjEnemies       = adjacentEnemiesOf bm unit coord
unitRound bm _ = bm

battleRound :: BattleMap -> BattleMap
battleRound bm = foldl unitRound bm sortedUnits
  where sortedUnits = sortBy (readingOrderCmp `on` fst) (allUnits bm)

battle :: BattleMap -> Int -> (BattleMap, Int)
battle bm round
  | some goblins && some elves =
      trace (displayMap bm) $ battle (battleRound bm) (succ round)
  | otherwise = (bm, round)
 where
  elves   = [ sq | sq@(_, SUnit Elf _) <- allUnits bm ]
  goblins = [ sq | sq@(_, SUnit Goblin _) <- allUnits bm ]
  some    = not . null

part1 :: BattleMap -> String
part1 = displayMap . fst . (`battle` 0)

main :: IO ()
main = interact (part1 . parseMap . lines)
