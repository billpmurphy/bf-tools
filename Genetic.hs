{-# LANGUAGE FlexibleInstances #-}

module Genetic where

import Control.Monad.Random

import Control.Monad
import Parser
import Interpreter

data ProblemType = Maximizing | Minimizing

data Optimization a = Optimization ProblemType (a -> Double)

type GenomeLength = Int
type Population a = [a]
type Fitness      = Double

data Phenotype a  = Phenotype Fitness a

class Genome a where
    randomEntity :: MonadRandom g => GenomeLength -> g a

    randomPool :: MonadRandom g => Int -> GenomeLength -> g (Population a)
    randomPool poolSize = sequence . replicate poolSize . randomEntity

    mutate :: MonadRandom g => a -> g a

    crossover :: MonadRandom g => a -> a -> g a

    fitness :: Optimization a -> a -> Fitness

    phenotype :: Optimization a -> a -> Phenotype a
    phenotype o x = Phenotype (fitness o x) x

    select :: MonadRandom g => Int -> Population (Phenotype a) -> g (Population a)

------------

instance Genome [Operator] where
    randomEntity i = sequence . replicate i $ getRandom
      where ops    = map snd operations
    mutate    = undefined
    crossover = undefined
    fitness   = undefined
    select    = undefined
