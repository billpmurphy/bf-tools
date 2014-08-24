{-# LANGUAGE FlexibleInstances #-}

module Interpreter (
    Program, Memory, Input, Output, Environment(..),
    start, step, run, runProgram, stepThrough
    ) where

import Data.Int (Int8)
import Data.Maybe (isNothing, fromMaybe)
import Prelude hiding (Left, Right)

import Parser
import Tape

data Environment = Environment { program :: Program, array  :: Memory
                               , input   :: Input,   output :: Output }

type Program = Tape (Maybe Operator)
type Memory  = Tape (Maybe Int8)
type Input   = [Int8]
type Output  = [Int8]

instance Show Environment where
    show (Environment p a i o) = show p
        ++ "\n" ++ show a ++ "\nInput: " ++ show i ++ " Output: " ++ show o

shift :: Bool -> Int8 -> Program -> Program
shift goBack n t@(Tape _ x _)
    | n == 0 && ((goBack && x == Just Open) || (not goBack && x == Just Close))
                      = right t
    | x == Just Open  = shift goBack (n+1) (next goBack t)
    | x == Just Close = shift goBack (n-1) (next goBack t)
    | otherwise       = shift goBack  n    (next goBack t)
  where next isLeft = if isLeft then left else right

start :: Input -> Program -> Environment
start i p = Environment (toStart p) (blankTape Nothing) i []
  where toStart t@(Tape (a:_) x _)
            | isNothing x || isNothing a = t
            | otherwise                  = toStart $ left t

step :: Environment -> Environment
step (Environment prog arr i o) = case focus prog of
    Just Plus  -> Environment (right prog) (inc arr)          i o
    Just Minus -> Environment (right prog) (dec arr)          i o
    Just Left  -> Environment (right prog) (zero $ left arr)  i o
    Just Right -> Environment (right prog) (zero $ right arr) i o
    Just Dot   -> Environment (right prog)  arr               i appendOut
    Just Comma -> Environment (right prog)  handleIn          nextIn o
    Just Open  -> Environment (jumpTo (==0) right) arr        i o
    Just Close -> Environment (jumpTo (>0)  left)  arr        i o
    Nothing    -> Environment  prog         arr               i o
  where jumpTo fn jump
            | fn $ fromMaybe 0 $ focus arr = shift (not $ fn 0) 0 (jump prog)
            | otherwise                    = right prog
        appendOut = o ++ [fromMaybe 0 $ focus arr]
        nextIn    = if null i then []  else tail i
        handleIn  = if null i then arr else set (Just $ head i) arr

run :: Environment -> Environment
run env = case focus $ program env of
    Nothing -> env
    Just op -> run $ step env

runProgram :: Input -> String -> Output
runProgram i = output . run . start i . listToTape . lexer

stepThrough :: Input -> String -> IO ()
stepThrough i = stepInto . start i . listToTape . lexer
  where stepInto e = case focus $ program e of
            Nothing -> print e
            Just op -> getLine >> print e >> putStrLn "\n" >> stepInto (step e)
