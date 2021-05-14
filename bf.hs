import Control.Monad
import Data.Char (ord, chr)
import Data.Word (Word8)

-- Implementation without IO. Basecase for future IO extension with MonadTransformers. 

-- ============================================================================
-- Data types for data and instruction tapes and their operations
-- ============================================================================

-- keep track of tape length with tuple (pointer position, tape length so far)
type Bound = (Int, Int)

leftB :: Bound -> Bound
leftB (x, y) = (x-1, y)

rightB :: Bound -> Bound
rightB (x, y) = (x+1, max y (x+1))

type Byte = Word8

byte2char :: Byte -> Char
byte2char = chr . fromEnum

-- a type for both the data tape and the instruction tape. 
-- pointer is head of first list. left of pointer is field1[1], right of pointer is field2[0]
-- [1 2 3 4 5 6]
--      ^
-- corresponds to Tape [3, 2, 1] [4, 5, 6] (2, 5)
data Tape = Tape [Byte] [Byte] Bound

zeroes :: [Byte]
zeroes = repeat 0

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

ptrPos :: Tape -> Int
ptrPos (Tape xs ys b) = fst b

tapeLength :: Tape -> Int
tapeLength (Tape xs ys b) = snd b

-- read from pointer position
readTape :: Tape -> Byte
readTape (Tape (ptr:xs) ys b) = ptr

-- is byte at ptr 0?
pointsToZero :: Tape -> Bool
pointsToZero = (==0) . readTape

pointsToChar :: Char -> Tape -> Bool
pointsToChar c = (==c) . byte2char . readTape

-- any operation on pointer position
onPtr :: (Byte -> Byte) -> Tape -> Tape
onPtr f (Tape (ptr:xs) ys b) = Tape ((f ptr):xs) ys b

-- write to pointer position
writeTape :: Byte -> Tape -> Tape
writeTape = onPtr . const

--increment, decrement the byte at ptr
inc :: Tape -> Tape
inc = onPtr (+1)

dec :: Tape -> Tape
dec = onPtr (\x -> x-1) 

instance Show Tape where
  show (Tape (ptr:xs) ys b) = show (reverse xs) ++ " " ++ show ptr ++ " " ++ show (take (snd b - fst b) ys)

-- for the instruction tape
show' (Tape (x:xx) yy b) = reverse xs ++ " " ++ [ptr] ++ " " ++ take (snd b - fst b) ys
  where ptr = byte2char x
        xs  = map byte2char xx
        ys  = map byte2char yy

-- ============================================================================
-- Instruction Tape: reuse tape but add some functions

-- find matching ], assuming program is syntactically correct, i.e., there IS a matching ] AND the tape points to a [ 
-- same with [ and left, so parametrized. result points to the bracket, so this needs a right/left shift afterwards, like every other instr.
seekAny dir charMatch charOther t = go 0 (dir t)
  where go n t
          | n == 0 && pointsToChar charMatch t = t
          | pointsToChar charMatch t           = go (n - 1) (dir t)
          | pointsToChar charOther t           = go (n + 1) (dir t)
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
  show (TM d i) = "\n" ++ show d ++ "\n" ++ show' i

testtm = TM (initTape [2,7]) (initStringTape "[->+<]")
testtm2 = TM emptyTape (initStringTape "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

-- ============================================================================
-- Put the data into a stateful context using State (-transformers) 
-- ============================================================================

-- TM state transformers
newtype State s a = State {runState :: s -> (a, s)}

-- State data constructor proxy
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

-- ============================================================================
-- The brainfuck operations are stateful
-- ============================================================================

-- let the monadic functions return effects. can be extended as necessary
data Effect = None | Result Byte

isNone None = True
isNone _    = False

-- these seem a bit verbose.. is this necessary? is there a better way?

-- shift data tape
fRight :: TM -> (Effect, TM)
fRight (TM d i) = (None , TM (right d) i)
sRight = state fRight

fLeft :: TM -> (Effect, TM)
fLeft (TM d i) = (None, TM (left d) i)
sLeft = state fLeft

-- increment byte at ptr on data tape
fInc :: TM -> (Effect, TM)
fInc (TM d i) = (None, TM (inc d) i)
sInc = state fInc

fDec :: TM -> (Effect, TM)
fDec (TM d i) = (None, TM (dec d) i)
sDec = state fDec

-- read or write byte from/to data tape
fOutp :: TM -> (Effect, TM)
fOutp tm = (Result (readTape $ dataTape tm), tm)
sOutp = state fOutp

fInp :: Byte -> TM -> (Effect, TM)
fInp byte (TM d i) = (None, TM (writeTape byte d) i)
sInp byte = state (fInp byte)

-- seek the next [ or the previous ]
fFwd :: TM -> (Effect, TM)
fFwd (TM d i)
  | pointsToZero d  = (None, TM d (seekRight i))
  | otherwise = (None, (TM d i))
sFwd = state fFwd

fBwd :: TM -> (Effect, TM)
fBwd (TM d i)
  | not $ pointsToZero d = (None, TM d (seekLeft i))
  | otherwise = (None, (TM d i))
sBwd = state fBwd

-- ============================================================================

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

-- ============================================================================
-- Putting it together: Run brainfuck turing machine step by step, 
-- collecting potential results (Effect)
-- ============================================================================

-- from Control.Monad.Loops
whileM :: Monad m => m Bool -> m a -> m [a]
whileM = whileM'

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

-- from instruction character, resolve which action to take next.
chooseAction :: Byte -> State TM Effect
chooseAction instr = 
  case byte2char $ instr of 
    '>' -> sRight
    '<' -> sLeft
    '+' -> sInc
    '-' -> sDec
    '.' -> sOutp --sOutp
    ',' -> return None --sInp -- to be combined with IO...
    '[' -> sFwd
    ']' -> sBwd
    _   -> return None

-- run the current step of the brainfuck program
stepTM :: State TM Effect
stepTM = do
  instr <- sReadInstr
  let next = chooseAction instr -- get statetransformer encoded by this char
  res <- next -- run that statetransformer
  sNext -- shift instruction tape pointer to right
  return res

-- combine a list of possibly empty effects into a string
combineEffects :: [Effect] -> [Char]
combineEffects ms = go "" ms 
  where
    go str [] = str
    go str ((None):xs) = go str xs
    go str ((Result byte):xs) = go (str ++ [(byte2char $ byte)]) xs

-- run a tm and print its result and states
runTM tm = (combineEffects (fst tup), (snd tup)) 
  where
    tup = runState (whileM sCheckNot stepTM) tm


