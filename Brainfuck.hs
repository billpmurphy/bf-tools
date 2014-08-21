module Brainfuck where

import Control.Comonad (Comonad(..))
import Prelude hiding (Left, Right)

-------------------------------------------------------------------------------
data Tape a = Tape [a] a [a]

instance Functor Tape where
    fmap f (Tape as x bs) = Tape (map f as) (f x) (fmap f bs)

left, right :: Tape a -> Tape a
left  (Tape (a:as) x bs) = Tape as a (x:bs)
right (Tape as x (b:bs)) = Tape (x:as) b bs

makeTape :: (a -> a) -> (a -> a) -> a -> Tape a
makeTape fl fr x = Tape (tail $ iterate fl x) x (tail $ iterate fr x)

instance Comonad Tape where
    duplicate = makeTape left right
    extract (Tape _ x _) = x

applyCenter :: (a -> a) -> Tape a -> Tape a
applyCenter f (Tape as x bs) = Tape as (f x) bs

inc, dec :: Tape Int -> Tape Int
inc = applyCenter (1+)
dec = applyCenter (1-)

-------------------------------------------------------------------------------

data Operator = Plus | Minus | Left | Right | Dot | Comma | Open | Close
    deriving (Eq)

type Program = [Operator]

data ProgramState = ProgramState (Tape (Maybe Operator)) (Tape (Maybe Int))

start :: Program -> ProgramState
start prog
    | null prog = ProgramState (Tape [] Nothing []) blankTape
    | otherwise = ProgramState programTape blankTape
  where programTape = Tape [Nothing] (head prog) $ tail prog ++ [Nothing]
        blankTape = Tape (repeat 0) 0 (repeat 0)
