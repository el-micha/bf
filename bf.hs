-- brainfuck interpreter

-- read file, tokenize it, check its well-formedness
-- this gives a list of instructions
-- needs a pointer, just as the tape needs a pointer

-- datatype tape with pointer with interface:
-- move pointer, read from pointer, write to pointer (input and output to and from pointer)

-- state of program is (tdata, tinstr) which will transition to (tdata', tinstr') after the instruction at the pointer of tinstr.
-- 


-- ==================================================================================

-- may need to keep track of rightmost pointer position, so we know how much to print and in order to see the range of a calculation
-- helper to keep track of tape size in performant way (without calling length xs everytime we go right)
-- pointer position, rightmost pointer position (so far)
type Bound = (Int, Int)
leftB :: Bound -> Bound
leftB (x, y) = (x-1, y)

rightB :: Bound -> Bound
rightB (x, y) = (x+1, max y (x+1))


-- test tape.. may suck

-- pointer is head of first tape. left of pointer is t1[1], right of pointer is t2[0]
-- [1 2 3 4 5 6]
--      ^
-- corresponds to Tape [3, 2, 1] [4, 5, 6] _
data Tape = Tape [Int] [Int] Bound

createTape :: Tape
createTape = Tape [0] [0 | _ <- [1..]] ((0, 0) :: Bound)

left :: Tape -> Tape
left (Tape [] _ _) = error "Cannot shift tape to left: Is at origin. Bad initialization."
left (Tape [x] _ _) = error "Cannot shift tape to left: Is at leftmost cell."
left (Tape (x:xs) ys b) = Tape xs (x:ys) (leftB b)

right :: Tape -> Tape
right (Tape xs (y:ys) b) = Tape (y:xs) ys (rightB b)

ptrPosition (Tape xs ys b) = (length xs) - 1
ptrPos (Tape xs ys b) = fst b
tapeLength (Tape xs ys b) = snd b

-- read from pointer position
readTape :: Tape -> Int
readTape (Tape (ptr:xs) ys b) = ptr

-- write to pointer position
writeTape :: Int -> Tape -> Tape
writeTape n (Tape (ptr:xs) ys b) = Tape (n:xs) ys b

instance Show Tape where
  show (Tape (ptr:xs) ys b) = show (reverse xs) ++ " " ++ show ptr ++ " " ++ show (take 10 ys)


