import Control.Monad
import Data.Char (ord, chr)
import Data.Word (Word8)
type Byte = Word8

-- keep track of tape length with tuple (pointer position, tape length so far)
type Bound = (Int, Int)
leftB :: Bound -> Bound
leftB (x, y) = (x-1, y)
rightB :: Bound -> Bound
rightB (x, y) = (x+1, max y (x+1))

-- a type for both the data tape and the instruction tape. 
-- pointer is head of first list. left of pointer is t1[1], right of pointer is t2[0]
-- [1 2 3 4 5 6]
--      ^
-- corresponds to Tape [3, 2, 1] [4, 5, 6] (2, 5)
data Tape = Tape [Byte] [Byte] Bound

zeroes = [0 | _ <- [1..]]

emptyTape :: Tape
emptyTape = Tape [0] zeroes ((0, 0) :: Bound)

initTape :: [Byte] -> Tape
initTape (x:xs) = Tape [x] (xs++zeroes) ((0, length xs) :: Bound)

-- shift pointer by one
left :: Tape -> Tape
left (Tape [] _ _) = error "Cannot shift tape to left: Is at origin. Bad initialization."
left (Tape [x] _ _) = error "Cannot shift tape to left: Is at leftmost cell."
left (Tape (x:xs) ys b) = Tape xs (x:ys) (leftB b)

right :: Tape -> Tape
right (Tape xs (y:ys) b) = Tape (y:xs) ys (rightB b)

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

--increment, decrement the byte at ptr
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

-- find matching ], assuming program is syntactically correct, i.e., there IS a matching ] AND the tape points to a [ 
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

-- init for instruction tape
initStringTape :: [Char] -> Tape
initStringTape = initTape . map (toEnum . ord)

-- the whole brainfuck program / machine is a turingmachine, where both tapes are in a state which can change with every instr.
data TM = TM {dataTape :: Tape, insTape :: Tape}

instance Show TM where
  show (TM d i) = show d ++ "\n" ++ show' i

testtm = TM (initTape [2,3]) (initStringTape "[->+<]")
testtm2 = TM emptyTape (initStringTape "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

-- TM state transformers
newtype State s a = State {runState :: s -> (a, s)}

-- the function is the data constructor of the type State
state :: (s -> (a, s)) -> State s a
state = State

-- canonical stuff
instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = state (\s -> (x, s)) 
  p >>= k = state $ \ s0 ->
    let (x, s1) = runState p s0
    in runState (k x) s1  

-- State TM or State Tape and then have 2 separate states?

-- define functions on TM as 
-- f :: TM -> (a, TM)
-- so they form a State TM a

-- these seem a bit verbose.. is this necessary? is there a better way?
-- TODO: they need to return NOTHING, not (). next instruction is unknown, so the return type is unknown -> Maybe
-- shift data tape
fRight :: TM -> ((), TM)
fRight (TM d i) = (() , TM (right d) i)
sRight = state fRight

fLeft :: TM -> ((), TM)
fLeft (TM d i) = ((), TM (left d) i)
sLeft = state fLeft

-- increment byte at ptr on data tape
fInc :: TM -> ((), TM)
fInc (TM d i) = ((), TM (inc d) i)
sInc = state fInc

fDec :: TM -> ((), TM)
fDec (TM d i) = ((), TM (dec d) i)
sDec = state fDec

-- read or write byte from/to data tape
fOutp :: TM -> (Byte, TM)
fOutp tm = (readTape $ dataTape tm, tm)
sOutp = state fOutp

fInp :: Byte -> TM -> ((), TM)
fInp byte (TM d i) = ((), TM (writeTape byte d) i)
sInp byte = state (fInp byte)

-- seek the next [ or the previous ]
fFwd :: TM -> ((), TM)
fFwd (TM d i)
  | isZero d  = ((), TM d (seekRight i))
  | otherwise = ((), (TM d i))
sFwd = state fFwd

fBwd :: TM -> ((), TM)
fBwd (TM d i)
  | not $ isZero d = ((), TM d (seekLeft i))
  | otherwise = ((), (TM d i))
sBwd = state fBwd

--read instruction from instr tape
fReadInstr :: TM -> (Byte, TM)
fReadInstr tm = (readTape $ insTape tm, tm)
sReadInstr = state fReadInstr

--next instruction: shift instr tape 
fNext :: TM -> ((), TM)
fNext (TM d i) = ((), TM d (right i))
sNext = state fNext

-- check if instruction tape is at the end
fCheck :: TM -> (Bool, TM)
fCheck (TM d i) = ((check i), TM d i)
  where check tape = (readTape tape) == 0
sCheck = state fCheck

sCheckNot = do 
  res <- sCheck
  return (not res)

--stepTM' :: State TM (Maybe Char)
--stepTM' = do
--  instr <- sReadInstr

chooseAction instr = 
  case chr . fromEnum $ instr of 
    '>' -> sRight
    '<' -> sLeft
    '+' -> sInc
    '-' -> sDec
    '.' -> return () --sOutp
    ',' -> return () --sInp
    '[' -> sFwd
    ']' -> sBwd
    _   -> return ()

stepTM :: State TM Char
stepTM = do
  instr <- sReadInstr
  let next = chooseAction instr -- get statetransformer encoded by this char
  next -- run that statetransformer
  sNext -- shift instruction tape pointer to right
  return (chr . fromEnum $ instr)

stepAndCheckTM = do
  stepTM
  ended <- sCheck
  when (not ended) $ do 
    stepAndCheckTM

-- untilM :: Monad m => m a -> m Bool -> m [a]



--  if ended then return ' ' 
--  else do stepAndCheckTM
  
-- to run:
-- runState stepAndCheckTM testtm 

-- from Control.Monad.Loops
whileM :: Monad m => m Bool -> m a -> m [a]
whileM = whileM'
-- |Execute an action repeatedly as long as the given boolean expression
-- returns True. The condition is evaluated before the loop body.
-- Collects the results into an arbitrary 'MonadPlus' value.
whileM' :: (Monad m, MonadPlus f) => m Bool -> m a -> m (f a)
whileM' p f = go
    where go = do
            x <- p
            if x
                then do
                        x  <- f
                        xs <- go
                        return (return x `mplus` xs)
                else return mzero












