{-# LANGUAGE FlexibleInstances #-}

module Tape (
    Tape(..), focus, set, left, right,
    makeTape, listToTape, blankTape,
    inc, dec, zero
    ) where

import Control.Comonad (Comonad(..))
import Data.Maybe (fromMaybe, catMaybes, isJust)

data Tape a = Tape [a] a [a]

focus :: Tape a -> a
focus (Tape _ x _) = x

set :: a -> Tape a -> Tape a
set d (Tape as x bs) = Tape as d bs

left, right :: Tape a -> Tape a
left  (Tape (a:as) x bs) = Tape as a (x:bs)
right (Tape as x (b:bs)) = Tape (x:as) b bs

makeTape :: (a -> a) -> (a -> a) -> a -> Tape a
makeTape fl fr x = Tape (tail $ iterate fl x) x (tail $ iterate fr x)

instance Functor Tape where
    fmap f (Tape as x bs) = Tape (fmap f as) (f x) (fmap f bs)

instance Comonad Tape where
    duplicate = makeTape left right
    extract = focus

listToTape :: [a] -> Tape (Maybe a)
listToTape []    = Tape (repeat Nothing)  Nothing (repeat Nothing)
listToTape (a:s) = Tape (repeat Nothing) (Just a) (map Just s ++ repeat Nothing)

blankTape :: a -> Tape a
blankTape d = Tape (repeat d) d (repeat d)

instance Show a => Show (Tape (Maybe a)) where
    show (Tape as x bs) = l ++ " " ++ m ++ " " ++ r
      where l = show . catMaybes . reverse $ takeWhile isJust as
            r = show . catMaybes           $ takeWhile isJust bs
            m = maybe "_" show x

inc, dec, zero :: Num a => Tape (Maybe a) -> Tape (Maybe a)
inc  (Tape as x bs) = Tape as (Just (fromMaybe 0 x + 1)) bs
dec  (Tape as x bs) = Tape as (Just (fromMaybe 0 x - 1)) bs
zero (Tape as x bs) = Tape as (Just (fromMaybe 0 x))     bs
