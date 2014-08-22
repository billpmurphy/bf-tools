module Parser (
    Operator(..), operations, parse, unparse, valid
    ) where

import Data.Tuple (swap)
import Data.Maybe (catMaybes)
import System.Random (Random(..))
import Prelude hiding (Left, Right)

data Operator = Plus | Minus | Left | Right | Dot | Comma | Open | Close
    deriving (Eq, Enum, Bounded, Show)

instance Random Operator where
    random g = case randomR (0, 7) g of
                (r, g') -> (toEnum r, g')
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

operations :: [(Char, Operator)]
operations = zip "+-<>.,[]" [minBound .. maxBound]

parse :: String -> [Operator]
parse = catMaybes . map (`lookup` operations)

unparse :: [Operator] -> String
unparse = catMaybes . map (`lookup` map swap operations)

valid :: [Operator] -> Bool
valid = walk 0
    where walk count []  = count == 0
          walk count (o:ops)
            | o == Open  = count >= 0 && walk (count+1) ops
            | o == Close = count >= 0 && walk (count-1) ops
            | otherwise  = count >= 0 && walk  count    ops
