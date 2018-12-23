-- | Day 19: Go With The Flow
--
-- A program is included to calculate the answer to Part One.
--
-- Because Part Two would run for many hours or days, the answer is
-- found by working backwards from the halting condition. First, let:
--
--   x = R0
--   y = R1
--   z = R2
--   counter = R3
--   a = R4
--   b = R5
--
-- Then consider the following observations:
--
-- * The program ends when a > z.
-- * z reaches a maximum value of 10551319, so the final a is 10551320.
-- * a is incremented by 1 in an outer loop while b is incremented in
--   an inner loop over the range 1..10551320 for each a:
--
--       FOR a IN 1..10551320:
--         FOR b IN 1..10551320:
--           ...
--
-- * The initial x is reset to 0.
-- * In the following, x is incremented by a whenever a * b == z:
--
--       03 mulr 4 5 1  // y = a * b
--       04 eqrr 1 2 1  // y = y == z
--       05 addr 1 3 3  // counter = y + counter
--       06 addi 3 1 3  // counter = counter + 1
--       07 addr 4 0 0  // x = a + x
--
-- * The final x is thus the sum of all divisors of 10551319.
--
--       x = 1 + 23 + 79 + 1817 + 5807 + 133561 + 458753 + 10551319
--       x = 11151360
--
-- Usage: runhaskell Day19.hs < ../inputs/19.txt

import           Data.Array                     ( Array
                                                , listArray
                                                , (!)
                                                , (//)
                                                )
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import           Data.Char                      ( toUpper )
import           Data.Maybe                     ( fromJust )
import           Data.Void                      ( Void )
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
            deriving (Show, Eq, Enum, Read)

type Instruction = (Opcode, Int, Int, Int)

type InstructionPointer = Int

type Program = (InstructionPointer, [Instruction])

type Parser = Parsec Void String

instruction :: Parser Instruction
instruction = do
  op        <- some lowerChar
  _         <- char ' '
  [a, b, c] <- some digitChar `sepBy` char ' '
  _         <- newline
  return (read $ capitalize op, read a, read b, read c)
  where
    capitalize s = toUpper (head s) : tail s

program :: Parser Program
program = do
  _            <- string "#ip "
  ip           <- some digitChar
  _            <- newline
  instructions <- many instruction
  return (read ip, instructions)

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

evalProgram :: RegisterBank -> Program -> RegisterBank
evalProgram regs (ip, instructions)
  | counter < 0 || counter >= length instructions = regs
  | otherwise = evalProgram regs'' (ip, instructions)
  where
    regs'' = regs' // [(ip, succ $ regs' ! ip)]
    regs' = eval regs (instructions !! counter)
    counter = regs ! ip

part1 :: Program -> String
part1 = show . evalProgram registers
  where
    registers = listArray (0, 5) [0, 0, 0, 0, 0, 0]

main :: IO ()
main = interact $ part1 . parse'
  where parse' = fromJust . parseMaybe program
