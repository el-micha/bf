-- brainfuck interpreter

-- read file, tokenize it, check its well-formedness
-- this gives a list of instructions
-- needs a pointer, just as the tape needs a pointer

-- datatype tape with pointer with interface:
-- move pointer, read from pointer, write to pointer (input and output to and from pointer)

-- state of program is (tdata, tinstr) which will transition to (tdata', tinstr') after the instruction at the pointer of tinstr.
-- 


-- ==================================================================================
import Control.Monad
import Data.Char (ord, chr)
import Data.Word (Word8)
type Byte = Word8

-- may need to keep track of rightmost pointer position, so we know how much to print and in order to see the range of a calculation
-- helper to keep track of tape size in performant way (without calling length xs everytime we go right)
-- (pointer position, tape length so far)
type Bound = (Int, Int)
leftB :: Bound -> Bound
leftB (x, y) = (x-1, y)

rightB :: Bound -> Bound
rightB (x, y) = (x+1, max y (x+1))

-- pointer is head of first list. left of pointer is t1[1], right of pointer is t2[0]
-- [1 2 3 4 5 6]
--      ^
-- corresponds to Tape [3, 2, 1] [4, 5, 6] _
data Tape = Tape [Byte] [Byte] Bound

emptyTape :: Tape
emptyTape = Tape [0] [0 | _ <- [1..]] ((0, 0) :: Bound)

initTape :: [Byte] -> Tape
initTape (x:xs) = Tape [x] xs ((0, length xs) :: Bound)

left :: Tape -> Tape
left (Tape [] _ _) = error "Cannot shift tape to left: Is at origin. Bad initialization."
left (Tape [x] _ _) = error "Cannot shift tape to left: Is at leftmost cell."
left (Tape (x:xs) ys b) = Tape xs (x:ys) (leftB b)

right :: Tape -> Tape
right (Tape xs (y:ys) b) = Tape (y:xs) ys (rightB b)

-- not performant
ptrPosition (Tape xs ys b) = (length xs) - 1
-- better
ptrPos (Tape xs ys b) = fst b
tapeLength (Tape xs ys b) = snd b

-- read from pointer position
readTape :: Tape -> Byte
readTape (Tape (ptr:xs) ys b) = ptr

-- is byte at ptr 0?
isZero :: Tape -> Bool
isZero = (==0) . readTape

isChar :: Char -> Tape -> Bool
isChar c = (==c) . chr . fromEnum . readTape

-- write to pointer position
writeTape :: Byte -> Tape -> Tape
writeTape n (Tape (ptr:xs) ys b) = Tape (n:xs) ys b

--increment, decrement
increment :: Tape -> Tape
increment (Tape (x:xs) y b) = Tape ((x+1):xs) y b
decrement :: Tape -> Tape
decrement (Tape (x:xs) y b) = Tape ((x-1):xs) y b
inc = increment
dec = decrement

instance Show Tape where
  show (Tape (ptr:xs) ys b) = show (reverse xs) ++ " " ++ show ptr ++ " " ++ show (take (snd b - fst b) ys)

-- for the instruction tape
show' (Tape (x:xx) yy b) = reverse xs ++ " " ++ [ptr] ++ " " ++ take (snd b - fst b) ys
  where ptr = f x
        xs  = map f xx
        ys  = map f yy
        f   = chr . fromEnum

-- =============================================================
-- instruction Tape: reuse tape but add some functions
-- Instruction a type which is represented by the 8 bf chars, but also is a state transformer on the (dtape, itape) tuple

-- find matching ], assuming program is syntactically correct, i.e., there IS a matching ] AND the tape points to a [ (charOther)
-- same with [ and left, so parametrized. result points to the bracket, so this needs a right/left afterwards, like every other instr.
seekAny dir charMatch charOther t = go 0 (dir t)
  where go n t
          | n == 0 && isChar charMatch t = t
          | isChar charMatch t           = go (n - 1) (dir t)
          | isChar charOther t           = go (n + 1) (dir t)
          | otherwise                    = go n (dir t)

seekRight :: Tape -> Tape
seekRight = seekAny right ']' '['
seekLeft :: Tape -> Tape
seekLeft  = seekAny left  '[' ']'

--initTape :: [Byte] -> Tape
initStringTape :: [Char] -> Tape
initStringTape = initTape . map (toEnum . ord)


data TuringMachine = TuringMachine {dataTape :: Tape, insTape :: Tape} deriving Show

-- read instruction from insTape
-- run instruction: affects state and IO
-- 

--newtype State s a = State {runstate :: s -> (a, s)}
























