-- Day 16: Chronal Classification --
--
-- Usage:
--
--   ghc -O3 Day16.hs
--   ./Day16 < ../inputs/16.txt

import           Data.Array                     ( Array
                                                , listArray
                                                , (!)
                                                , (//)
                                                )
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.List                      ( delete )
import           Data.Void                      ( Void )
import           Control.Monad                  ( foldM )
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Register = Int

type RegisterBank = Array Int Register

data Opcode = Addr
            | Addi
            | Mulr
            | Muli
            | Banr
            | Bani
            | Borr
            | Bori
            | Setr
            | Seti
            | Gtir
            | Gtri
            | Gtrr
            | Eqir
            | Eqri
            | Eqrr
            deriving (Show, Eq, Enum)

type Input = Int
type Output = Int
type OpcodeId = Int
type Instruction = (Opcode, Input, Input, Output)
type RawInstruction = (OpcodeId, Input, Input, Output)
type Program = [RawInstruction]

type Sample = (RegisterBank, RawInstruction, RegisterBank)

type Parser = Parsec Void String


-- OPCODES AND EVALUATION

allOps :: [Opcode]
allOps = [Addr ..]

initialRegisters :: RegisterBank
initialRegisters = listArray (0, 3) [0, 0, 0, 0]

eval :: RegisterBank -> Instruction -> RegisterBank
eval regs (Addr, a, b, c) = regs // [(c, regs ! a + regs ! b)]
eval regs (Addi, a, b, c) = regs // [(c, regs ! a + b)]
eval regs (Mulr, a, b, c) = regs // [(c, regs ! a * regs ! b)]
eval regs (Muli, a, b, c) = regs // [(c, regs ! a * b)]
eval regs (Banr, a, b, c) = regs // [(c, regs ! a .&. regs ! b)]
eval regs (Bani, a, b, c) = regs // [(c, regs ! a .&. b)]
eval regs (Borr, a, b, c) = regs // [(c, regs ! a .|. regs ! b)]
eval regs (Bori, a, b, c) = regs // [(c, regs ! a .|. b)]
eval regs (Setr, a, _, c) = regs // [(c, regs ! a)]
eval regs (Seti, a, _, c) = regs // [(c, a)]
eval regs (Gtir, a, b, c) = regs // [(c, if a > regs ! b then 1 else 0)]
eval regs (Gtri, a, b, c) = regs // [(c, if regs ! a > b then 1 else 0)]
eval regs (Gtrr, a, b, c) = regs // [(c, if regs ! a > regs ! b then 1 else 0)]
eval regs (Eqir, a, b, c) = regs // [(c, if a == regs ! b then 1 else 0)]
eval regs (Eqri, a, b, c) = regs // [(c, if regs ! a == b then 1 else 0)]
eval regs (Eqrr, a, b, c) = regs // [(c, if regs ! a == regs ! b then 1 else 0)]


-- INPUT PARSING

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

registers :: Parser RegisterBank
registers = do
  regs <- brackets $ some digitChar `sepBy` string ", "
  return $ listArray (0, 3) (map read regs)

instruction :: Parser RawInstruction
instruction = do
  [op, a, b, c] <- some digitChar `sepBy` char ' '
  _             <- newline
  return (read op, read a, read b, read c)

sample :: Parser Sample
sample = do
  _        <- string "Before: "
  before   <- registers
  _        <- newline
  instruct <- instruction
  _        <- string "After:  "
  after    <- registers
  _        <- some newline
  return (before, instruct, after)

samplesWithProgram :: Parser ([Sample], Program)
samplesWithProgram = do
  samples <- many sample
  _       <- many newline
  program <- many instruction
  return (samples, program)


-- CONSTRAINT SATISFACTION

-- | Checks a sample for correctness with a particular opcode
check :: Sample -> Opcode -> Bool
check (before, (_, a, b, c), after) op = eval before (op, a, b, c) == after

-- | Checks all samples for correctness, given an opcode sequence {0..n}
checkAll :: [Sample] -> [Opcode] -> Bool
checkAll [] _ = True
checkAll (s@(_, (n, _, _, _), _) : samples) ops
  | check s (ops !! n) = checkAll samples ops
  | otherwise          = False

-- | Find valid opcode sequence(s) as constrained by the given samples.
solve :: [Sample] -> [[Opcode]]
solve samples = map (reverse . fst) $ foldM addOpcode ([], allOps) allOps
  where
    addOpcode (xs, candidates) _ =
      [ (x : xs, delete x candidates) | x <- candidates, checkPrefix (x : xs) ]
    checkPrefix ops = checkAll (samplesUnder $ length ops) (reverse ops)
    samplesUnder n = filter (\(_, (x, _, _, _), _) -> x < n) samples

part1 :: ([Sample], Program) -> String
part1 = show . length . filter ((> 2) . length) . map passed . fst
  where passed s = filter id $ map (check s) allOps

part2 :: ([Sample], Program) -> String
part2 (samples, program) = show
  $ foldl eval initialRegisters (map toOp program)
  where
    toOp (op, a, b, c) = (solved !! op, a, b, c)
    solved = head $ solve samples

main :: IO ()
main = interact (\input -> unlines [part1 (parse' input), part2 (parse' input)])
  where parse' = fromJust . parseMaybe samplesWithProgram
