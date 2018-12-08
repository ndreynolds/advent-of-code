-- Day 7: The Sum of Its Parts  --
--
-- Usage: runhaskell Day07.hs < ../inputs/07.txt
--

import           Prelude                 hiding ( seq )
import           Data.Char                      ( ord )
import           Data.Set                       ( Set
                                                , isSubsetOf
                                                )
import qualified Data.Set                      as Set
import           Data.List                      ( delete
                                                , find
                                                , nub
                                                , partition
                                                , sort
                                                )
import           Data.List.Split                ( splitOn )

type Step = Char
type Dependency = (Step, Step)

parseDep :: String -> Dependency
parseDep str = (step, dependsOn)
  where [[dependsOn], [step]] = filter ((== 1) . length) $ splitOn " " str

depsFor :: Step -> [Dependency] -> [Step]
depsFor step ((s, d) : rest) | step == s = d : depsFor step rest
                             | otherwise = depsFor step rest
depsFor _ [] = []

stepsWithDeps :: [Dependency] -> [(Step, Set Step)]
stepsWithDeps deps =
  [ (step, Set.fromList $ depsFor step deps) | step <- uniqueSteps ]
  where uniqueSteps = sort $ nub $ map fst deps ++ map snd deps

buildSeq :: [Step] -> [(Step, Set Step)] -> [Step]
buildSeq seq []    = reverse seq
buildSeq seq steps = buildSeq (step : seq) rest
  where (Just (step, _), rest) = removeFirst (satisfied seq) steps

removeFirst :: Eq a => (a -> Bool) -> [a] -> (Maybe a, [a])
removeFirst predicate xs = case find predicate xs of
  Just x  -> (Just x, delete x xs)
  Nothing -> (Nothing, xs)

satisfied :: [Step] -> (Step, Set Step) -> Bool
satisfied seq (_step, deps) = deps `isSubsetOf` Set.fromList seq

timeRequired :: Step -> Int
timeRequired step = ord step - 4

workerCount :: Int
workerCount = 5

work :: Int -> [(Step, Int)] -> [Step] -> [(Step, Set Step)] -> Int
work elapsedSec workload completed incomplete
  | null incomplete && null newWorkload = elapsedSec
  | workload == newWorkload = work (succ elapsedSec)
                                   workload
                                   completed
                                   incomplete
  | otherwise = work elapsedSec newWorkload newCompleted newIncomplete
 where
  newWorkload = [ (step, timeFinished step) | step <- newTasks ] ++ workloadWithoutFinished
  newIncomplete = filter (not . (`elem` newCompleted) . fst) incomplete
  newCompleted = completed ++ (map fst finishedTasks)
  newTasks  = map fst $ take extraCapacity available

  extraCapacity = workerCount - length workloadWithoutFinished
  available =
    filter (\t -> not (inWorkload t) && satisfied completed t) incomplete
  (workloadWithoutFinished, finishedTasks) =
    partition ((> elapsedSec) . snd) workload

  timeFinished = (+ elapsedSec) . timeRequired
  inWorkload (step, _) = any ((== step) . fst) workload

part1 :: [Dependency] -> String
part1 deps = buildSeq [] (stepsWithDeps deps)

part2 :: [Dependency] -> String
part2 deps = show $ work 0 [] [] seqWithSteps
 where
  seqWithSteps = [ (step, Set.fromList $ depsFor step deps) | step <- seq ]
  seq          = buildSeq [] (stepsWithDeps deps)

main :: IO ()
main = interact (\input -> unlines [part1 $ parse input, part2 $ parse input])
  where parse = map parseDep . lines
