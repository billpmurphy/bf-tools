{-# LANGUAGE FlexibleInstances #-}

module Genetic where

import Control.Monad.Random

import Control.Monad
import Data.List (sortBy)
import Data.Ord (comparing)
import Parser
import Interpreter

data ProblemType = Maximizing | Minimizing

data Optimization a = Optimization { problem :: ProblemType, fn :: a -> Double }

type GenomeLength = Int
type Population a = [a]
type Fitness      = Double

data Phenotype a  = Phenotype { fitness :: Fitness, genotype :: a }

class Genome a where
    randomEntity :: MonadRandom g => GenomeLength -> g a

    randomPool :: MonadRandom g => Int -> GenomeLength -> g (Population a)
    randomPool poolSize = replicateM poolSize . randomEntity

    mutate :: MonadRandom g => Double -> a -> g a

    crossover :: MonadRandom g => a -> a -> g a

    phenotype :: Optimization a -> a -> Phenotype a
    phenotype = fn

    selectElite :: MonadRandom g => Population (Phenotype a) -> g (Population a)

selectTopN:: Int -> Population (Phenotype a) -> g (Population a)
selectTopN i = take i . sortBy (comparing fitness)
------------

instance Random Operator where
    random g = case randomR (0, 7) g of
                (r, g') -> (toEnum r, g')
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

instance Genome [Operator] where
    randomEntity i = do
        p <- replicateM i getRandom
        if valid p then return p else randomEntity i

    mutate       = undefined
    crossover    = undefined
    selectElite  = undefined
