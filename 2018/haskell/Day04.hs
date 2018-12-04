-- Day 4: Repose Record --
--
-- Requires the megaparsec package.
--
-- Usage: runhaskell Day04.hs < ../inputs/04.txt

import Data.Function                  ( on )
import Data.List                      ( sort, group, groupBy, maximumBy )
import Data.Maybe                     ( fromJust )
import Data.Void
import Data.Map                       ( Map, fromListWith, toList )
import Text.Megaparsec
import Text.Megaparsec.Char

data Shift = Shift Integer [ShiftEvent] deriving Show

data ShiftEvent = Asleep Minutes
                | Awake Minutes
                deriving Show

type Minutes = Integer

type Parser = Parsec Void String

minutes :: Parser Minutes
minutes = do
  char '['
  skipSomeTill (digitChar <|> char '-' <|> spaceChar) (char ':')
  min <- count 2 digitChar
  char ']'
  return $ read min

slumber :: Parser [ShiftEvent]
slumber = do
  asleepAt <- minutes
  string " falls asleep\n"
  awakeAt <- minutes
  string " wakes up\n"
  return $ [Asleep asleepAt, Awake awakeAt]

shift :: Parser Shift
shift = do
  _start <- minutes
  string " Guard #"
  guardId <- some digitChar
  string " begins shift\n"
  events <- many (try slumber)
  return $ Shift (read guardId) (concat events)

shifts :: Parser [Shift]
shifts = many shift

minAsleep :: Shift -> Integer
minAsleep (Shift id ((Asleep start) : (Awake end) : rest)) =
  end - start + minAsleep (Shift id rest)
minAsleep (Shift _ []) = 0

asleepMinutes :: Shift -> [Integer]
asleepMinutes (Shift id ((Asleep start) : (Awake end) : rest)) =
  [start .. end - 1] ++ asleepMinutes (Shift id rest)
asleepMinutes (Shift _ []) = []

mostFrequent :: Ord a => [a] -> a
mostFrequent = head . maximumBy (compare `on` length) . group . sort

highestFrequency :: Ord a => [a] -> Int
highestFrequency [] = 0
highestFrequency xs = (maximum . map length . group . sort) xs

shiftsByGuardId :: [Shift] -> Map Integer [Shift]
shiftsByGuardId shifts =
  fromListWith (++) [ (guardId, [shift]) | shift@(Shift guardId _) <- shifts ]

sleepiestMin :: [Shift] -> Integer
sleepiestMin shifts = mostFrequent $ concat $ map asleepMinutes shifts

part1 :: [Shift] -> String
part1 shifts = show $ sleepyGuard * (sleepiestMin sleepyShifts)
 where
  (sleepyGuard, sleepyShifts) =
    maximumBy (compare `on` sum . map minAsleep . snd) (toList $ shiftsByGuardId shifts)

part2 :: [Shift] -> String
part2 shifts = show $ sleepyGuard * (sleepiestMin sleepyShifts)
 where
  (sleepyGuard, sleepyShifts) = maximumBy
    (compare `on` highestFrequency . concat . map asleepMinutes . snd)
    (toList $ shiftsByGuardId shifts)

main :: IO ()
main = interact
  (\input -> unlines [part1 (parseShifts input), part2 (parseShifts input)])
 where
  parseShifts = fromJust . parseMaybe shifts . sortInput
  sortInput   = unlines . sort . lines
