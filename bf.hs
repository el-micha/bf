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
  show (TM d i) = "\n" ++ show d ++ "\n" ++ show' i

testtm = TM (initTape [2,7]) (initStringTape "[->+<]")
testtm2 = TM emptyTape (initStringTape "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

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
  | isZero d  = (None, TM d (seekRight i))
  | otherwise = (None, (TM d i))
sFwd = state fFwd

fBwd :: TM -> (Effect, TM)
fBwd (TM d i)
  | not $ isZero d = (None, TM d (seekLeft i))
  | otherwise = (None, (TM d i))
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
            return (xs `mplus` return x)
          else return mzero

-- let the monadic functions return effects. can be extended as necessary
data Effect = None | Result Byte
isNone None = True
isNone _    = False
fromResult (Result c) = c
fromResult _ = error "None has no result"

-- from instruction char, resolve which action to take next.
chooseAction :: Enum a => a -> State TM Effect
chooseAction instr = 
  case chr . fromEnum $ instr of 
    '>' -> sRight
    '<' -> sLeft
    '+' -> sInc
    '-' -> sDec
    '.' -> sOutp --sOutp
    ',' -> return None --sInp
    '[' -> sFwd
    ']' -> sBwd
    _   -> return None

-- run the current step of the program
stepTM :: State TM Effect
stepTM = do
  instr <- sReadInstr
  let next = chooseAction instr -- get statetransformer encoded by this char
  res <- next -- run that statetransformer
  sNext -- shift instruction tape pointer to right
  return res

-- combine a list of possibly empty effects into a string
unpack :: [Effect] -> [Char]
unpack ms = go "" ms 
  where
    go str [] = str
    go str ((None):xs) = go str xs
    go str ((Result byte):xs) = go ((chr . fromEnum $ byte):str) xs

-- run a tm and print its result and states
runTM tm = (unpack (fst tup), (snd tup)) 
  where
    tup = runState (whileM sCheckNot stepTM) tm















