-- VM.hs
module VM
  ( Machine(..)
  , emptyMachine
  , runPure       -- pure interpreter: program + input bytes -> output bytes
  ) where

import Data.Word (Word8)
import Grammar

-- A simple tape machine: ... left stack | cell | right stack ...
data Machine = Machine
  { left :: [Word8]
  , cell :: Word8
  , right :: [Word8]
  } deriving (Show, Eq)

emptyMachine :: Machine
emptyMachine = Machine [] 0 []

-- Pointer moves
moveR :: Machine -> Machine
moveR (Machine l c (r:rs)) = Machine (c:l) r rs
moveR (Machine l c [])     = Machine (c:l) 0 []

moveL :: Machine -> Machine
moveL (Machine (x:xs) c r) = Machine xs x (c:r)
moveL (Machine []     c r) = Machine [] 0 (c:r)

-- Cell ops (Word8 wraps automatically)
incVal :: Machine -> Machine
incVal m = m { cell = cell m + 1 }

decVal :: Machine -> Machine
decVal m = m { cell = cell m - 1 }

setCell :: Word8 -> Machine -> Machine
setCell v m = m { cell = v }

-- The core executor. We keep input and output as difference lists
-- for efficiency; here we just carry output as reversed [Word8].
runPure :: [Instruction] -> [Word8] -> [Word8]
runPure prog inputBytes =
  let (_, _, outRev) = execBlock prog emptyMachine inputBytes []
  in reverse outRev

-- Execute a block of instructions
-- Arguments: program, machine, input, outRev
-- Returns:   (machine', input', outRev')
execBlock :: [Instruction] -> Machine -> [Word8] -> [Word8]
          -> (Machine, [Word8], [Word8])
execBlock [] m inp out = (m, inp, out)
execBlock (i:is) m inp out =
  case i of
    IncDataPoint -> execBlock is (moveR m) inp out
    DecDataPoint -> execBlock is (moveL m) inp out
    IncDataVal   -> execBlock is (incVal m) inp out
    DecDataVal   -> execBlock is (decVal m) inp out
    OutDataVal   -> execBlock is m inp (cell m : out)
    AcceptOneByteIn ->
      let (b, inp') = case inp of
                        (x:xs) -> (x, xs)
                        []     -> (0, [])       -- EOF -> 0 (BF semantics)
      in execBlock is (setCell b m) inp' out
    WhileDataValNonZero body ->
      let (m', inp', out') = loopWhileNZ body m inp out
      in execBlock is m' inp' out'

-- While loop: execute body repeatedly while current cell != 0
loopWhileNZ :: [Instruction] -> Machine -> [Word8] -> [Word8]
            -> (Machine, [Word8], [Word8])
loopWhileNZ body m inp out
  | cell m == 0 = (m, inp, out)
  | otherwise   =
      let (m1, inp1, out1) = execBlock body m inp out
      in loopWhileNZ body m1 inp1 out1
