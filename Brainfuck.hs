{-# LANGUAGE FlexibleInstances #-}

--todo: Dot and Comma

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Int (Int8)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Prelude hiding (Left, Right)

-------------------------------------------------------------------------------

data Tape a = Tape [a] a [a]

left, right :: Tape a -> Tape a
left  (Tape (a:as) x bs) = Tape as a (x:bs)
right (Tape as x (b:bs)) = Tape (x:as) b bs

instance Show a => Show (Tape (Maybe a)) where
    show (Tape as x bs) = l ++ " " ++ m ++ " " ++ r
      where l = show . catMaybes . reverse $ takeWhile isJust as
            r = show . catMaybes           $ takeWhile isJust bs
            m = fromMaybe "_" $ fmap show x

inc, dec, zero :: Tape (Maybe Int8) -> Tape (Maybe Int8)
inc  (Tape as x bs) = Tape as (Just (fromMaybe 0 x + 1)) bs
dec  (Tape as x bs) = Tape as (Just (fromMaybe 0 x - 1)) bs
zero (Tape as x bs) = Tape as (Just (fromMaybe 0 x))     bs

-------------------------------------------------------------------------------

data Operator = Plus | Minus | Left | Right | Dot | Comma | Open | Close
    deriving (Eq, Show, Enum)

type Program = Tape (Maybe Operator)
type Memory  = Tape (Maybe Int8)

parse :: String -> Program
parse input
    | null ops  = Tape (repeat Nothing)  Nothing   (repeat Nothing)
    | otherwise = Tape (repeat Nothing) (head ops) (tail ops ++ repeat Nothing)
  where ops = map Just . catMaybes $ map (flip lookup operations) input
        operations = zip "+-<>.,[]" [(Plus)..(Close)]

-------------------------------------------------------------------------------

data Environment = Environment { program :: Program, array :: Memory }

instance Show Environment where
    show (Environment p a) = show p ++ "\n" ++ show a

start :: Program -> Environment
start p = Environment (toStart p) blankTape
  where toStart t@(Tape (a:_) x _)
            | isNothing x || isNothing a = t
            | otherwise                  = toStart $ left t
        blankTape = Tape (repeat Nothing) (Just 0) (repeat Nothing)

step :: Environment -> Environment
step (Environment prog@(Tape _ x _) arr@(Tape _ i _)) = case x of
    Just Plus  -> Environment (right prog)  (inc arr)
    Just Minus -> Environment (right prog)  (dec arr)
    Just Left  -> Environment (right prog)  (zero $ left arr)
    Just Right -> Environment (right prog)  (zero $ right arr)
    Just Dot   -> undefined
    Just Comma -> undefined
    Just Open  -> Environment (jumpTo False) arr
    Just Close -> Environment (jumpTo True)  arr
    _          -> Environment  prog          arr
  where jumpTo back
            | (i == Just 0 && back) || (i /= Just 0 && not back) = right prog
            | otherwise = shift back 0 (next back $ prog)
        shift back n t@(Tape _ x _)
            | n == 0 && ((back     && x == Just Open )
                      || (not back && x == Just Close)) = right t
            | x == Just Open  = shift back (n+1) (next back $ t)
            | x == Just Close = shift back (n-1) (next back $ t)
            | otherwise       = shift back  n    (next back $ t)
        next direction = if direction then left else right

run :: Environment -> Environment
run env@(Environment (Tape _ x _) _) = case x of
    Nothing -> env
    Just op -> run $ step env

runProgram :: String -> Memory
runProgram = array . run . start . parse

stepThrough :: String -> IO ()
stepThrough p = let environment = start $ parse p in stepInto environment
  where stepInto env@(Environment (Tape _ x _) _) = case x of
            Nothing -> print env
            Just op -> print env >> putStrLn "\n" >> (stepInto $ step env)
