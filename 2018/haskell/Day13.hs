-- Day 13: Mine Cart Madness --
--
-- Usage:
-- 
--   ghc -O3 Day13.hs
--   ./Day13 < ../inputs/13.txt

import           Data.List                      ( find
                                                , groupBy
                                                , intercalate
                                                , sortOn
                                                , maximumBy
                                                , intersect
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import           Data.Ord                       ( comparing )
import           Data.Tuple                     ( swap )


-- TYPES

data Track = Track TrackType TrackState
           deriving (Show, Eq)

data TrackType = TVertical     -- |
               | THorizontal   -- -
               | TDiagonalUp   -- /
               | TDiagonalDown -- \
               | TIntersection -- +
               deriving (Show, Eq)

data TrackState = SCart CartDirection IntersectionRule
                | SCrash
                | SClear
                deriving (Show, Eq)

data CartDirection = DUp
                   | DDown
                   | DLeft
                   | DRight
                   deriving (Show, Eq)

data IntersectionRule = RLeft
                      | RStraight
                      | RRight
                      deriving (Show, Eq)

type Coord = (Integer, Integer)

type TrackNetwork = Map Coord Track


-- PARSING

parseTrack :: Char -> Track
parseTrack '|'  = Track TVertical SClear
parseTrack '-'  = Track THorizontal SClear
parseTrack '/'  = Track TDiagonalUp SClear
parseTrack '\\' = Track TDiagonalDown SClear
parseTrack '+'  = Track TIntersection SClear
parseTrack '^'  = Track TVertical $ SCart DUp RLeft
parseTrack 'v'  = Track TVertical $ SCart DDown RLeft
parseTrack '<'  = Track THorizontal $ SCart DLeft RLeft
parseTrack '>'  = Track THorizontal $ SCart DRight RLeft
parseTrack x    = error $ "parse error at: " ++ show x

parseLine :: Coord -> String -> [(Coord, Track)]
parseLine (x, y) (' ' : xs) = parseLine (x + 1, y) xs
parseLine (x, y) (t   : xs) = ((x, y), parseTrack t) : parseLine (x + 1, y) xs
parseLine _      []         = []


-- CART TRANSITIONS

nextTrack :: CartDirection -> Coord -> TrackNetwork -> (Coord, Track)
nextTrack dir (x, y) network = (coord, fromJust $ Map.lookup coord network)
 where
  coord = case dir of
    DUp    -> (x, y - 1)
    DDown  -> (x, y + 1)
    DLeft  -> (x - 1, y)
    DRight -> (x + 1, y)

applyWithCrashState :: TrackState -> Track -> Track -> Track
applyWithCrashState crashState (Track _ (SCart dir rule)) track = case track of
  crashSite@(Track _ SCrash)           -> crashSite
  Track      t             (SCart _ _) -> Track t crashState
  Track      TDiagonalUp   SClear      -> case dir of
    DLeft  -> Track TDiagonalUp (SCart DDown rule)
    DRight -> Track TDiagonalUp (SCart DUp rule)
    DUp    -> Track TDiagonalUp (SCart DRight rule)
    DDown  -> Track TDiagonalUp (SCart DLeft rule)
  Track TDiagonalDown SClear -> case dir of
    DLeft  -> Track TDiagonalDown (SCart DUp rule)
    DRight -> Track TDiagonalDown (SCart DDown rule)
    DUp    -> Track TDiagonalDown (SCart DLeft rule)
    DDown  -> Track TDiagonalDown (SCart DRight rule)
  Track t SClear -> Track t (SCart dir rule)

moveCart :: (Coord, Track) -> (Coord, Track) -> TrackNetwork -> TrackNetwork
moveCart (coord, Track prevType _) to = Map.union updatedEntries
 where
  updatedEntries  = Map.fromList [prevWithoutCart, to]
  prevWithoutCart = (coord, Track prevType SClear)

nextIntersectionRule :: IntersectionRule -> IntersectionRule
nextIntersectionRule rule = case rule of
  RLeft     -> RStraight
  RStraight -> RRight
  RRight    -> RLeft

move
  :: (Track -> Track -> Track) -> (Coord, Track) -> TrackNetwork -> TrackNetwork
move apply from@(coord, Track TIntersection (SCart dir rule)) network =
  moveCart from to network
 where
  to                = (coord', apply fromTrack toTrack)
  (coord', toTrack) = nextTrack newDir coord network
  fromTrack         = Track TIntersection (SCart newDir newRule)
  newRule           = nextIntersectionRule rule
  newDir            = case (dir, rule) of
    (DDown     , RLeft    ) -> DRight
    (DDown     , RRight   ) -> DLeft
    (DUp       , RLeft    ) -> DLeft
    (DUp       , RRight   ) -> DRight
    (DLeft     , RLeft    ) -> DDown
    (DLeft     , RRight   ) -> DUp
    (DRight    , RLeft    ) -> DUp
    (DRight    , RRight   ) -> DDown
    (currentDir, RStraight) -> currentDir
move apply from@(coord, fromTrack@(Track _ (SCart dir _))) network = moveCart
  from
  to
  network
 where
  to                = (coord', apply fromTrack toTrack)
  (coord', toTrack) = nextTrack dir coord network
move _ (_, Track _ _) network = network


-- DISPLAY

displayTrackNetwork :: TrackNetwork -> String
displayTrackNetwork network =
  intercalate
      "\n"
      [ displayRow row
      | row <- groupBy sameRow $ sortOn (snd . fst) $ Map.toList network
      ]
    ++ "\n"
    ++ show carts
 where
  sameRow ((_, y1), _) ((_, y2), _) = y1 == y2
  carts = occupiedTracks network

displayRow :: [(Coord, Track)] -> String
displayRow row = [ displayTrack (trackAt (x, y)) | x <- [0 .. xMax] ]
 where
  trackAt (x', y') = snd <$> find ((== (x', y')) . fst) row
  ((xMax, _), _) = maximumBy (comparing (fst . fst)) row
  (_        , y) = fst $ head row

displayTrack :: Maybe Track -> Char
displayTrack (Just (Track _ SCrash))           = 'X'
displayTrack (Just (Track _ (SCart DUp _)))    = '^'
displayTrack (Just (Track _ (SCart DDown _)))  = 'v'
displayTrack (Just (Track _ (SCart DLeft _)))  = '<'
displayTrack (Just (Track _ (SCart DRight _))) = '>'
displayTrack (Just (Track THorizontal _))      = '-'
displayTrack (Just (Track TVertical _))        = '|'
displayTrack (Just (Track TDiagonalDown _))    = '\\'
displayTrack (Just (Track TDiagonalUp _))      = '/'
displayTrack (Just (Track TIntersection _))    = '+'
displayTrack Nothing                           = ' '


-- PARTS

occupiedTracks :: TrackNetwork -> [(Coord, Track)]
occupiedTracks network =
  sortOn (swap . fst) [ t | t@(_, Track _ (SCart _ _)) <- Map.toList network ]

tick
  :: (Track -> Track -> Track)
  -> TrackNetwork
  -> [(Coord, Track)]
  -> TrackNetwork
tick _     network []             = network
tick apply network (cart : carts) = tick
  apply
  network'
  (carts `intersect` occupiedTracks network')
  where network' = move apply cart network

firstCrash :: TrackNetwork -> (Coord, Track)
firstCrash network
  | null crashes = firstCrash $ tick apply network (occupiedTracks network)
  | otherwise    = head crashes
 where
  crashes =
    sortOn (swap . fst) [ t | t@(_, Track _ SCrash) <- Map.toList network ]
  apply = applyWithCrashState SCrash

lastCart :: TrackNetwork -> Maybe (Coord, Track)
lastCart network = case carts of
  []     -> Nothing
  [cart] -> Just cart
  _      -> lastCart $ tick apply network carts
 where
  carts = occupiedTracks network
  apply = applyWithCrashState SClear

part1 :: TrackNetwork -> String
part1 = show . firstCrash

part2 :: TrackNetwork -> String
part2 = show . lastCart

main :: IO ()
main = interact (\input -> unlines [part1 $ parse input, part2 $ parse input])
 where
  parse = Map.fromList . concatMap parseRow . zip [0 ..] . lines
  parseRow (y, line) = parseLine (0, y) line
