-- brainfuck interpreter

-- read file, tokenize it, check its well-formedness
-- this gives a list of instructions
-- needs a pointer, just as the tape needs a pointer

-- datatype tape with pointer with interface:
-- move pointer, read from pointer, write to pointer (input and output to and from pointer)

-- state of program is (tdata, tinstr) which will transition to (tdata', tinstr') after the instruction at the pointer of tinstr.
-- 


-- ==================================================================================

-- test tape.. may suck

-- pointer is head of first tape. left of pointer is t1[1], right of pointer is t2[0]
-- [1 2 3 4 5 6]
--      ^
-- corresponds to Tape [3, 2, 1] [4, 5, 6]
data Tape = Tape [Int] [Int]

createTape :: Tape
createTape = Tape [0] [0 | _ <- [1..]]

left :: Tape -> Tape
left (Tape [] _) = error "Cannot shift tape to left: Is at origin. Bad initialization."
left (Tape [x] _) = error "Cannot shift tape to left: Is at leftmost cell."
left (Tape (x:xs) ys) = Tape xs (x:ys)

right :: Tape -> Tape
right (Tape xs (y:ys)) = Tape (y:xs) ys

ptrPosition (Tape xs ys) = (length xs) - 1

-- read from pointer position
readTape :: Tape -> Int
readTape (Tape (ptr:xs) ys) = ptr

-- write to pointer position
writeTape :: Int -> Tape -> Tape
writeTape n (Tape (ptr:xs) ys) = Tape (n:xs) ys

instance Show Tape where
  show (Tape (ptr:xs) ys) = show (reverse xs) ++ " " ++ show ptr ++ " " ++ show (take 10 ys)

