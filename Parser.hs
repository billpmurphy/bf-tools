module Parser ( Operator(..), operations, lexer, unlexer, valid
    ) where

import Data.Tuple (swap)
import Data.Maybe (mapMaybe)
import System.Random (Random(..))
import Prelude hiding (Left, Right)

data Operator = Plus | Minus | Left | Right | Dot | Comma | Open | Close
    deriving (Eq, Enum, Bounded, Show)

operations :: [(Char, Operator)]
operations = zip "+-<>.,[]" [minBound .. maxBound]

lexer :: String -> [Operator]
lexer = mapMaybe (`lookup` operations)

valid :: [Operator] -> Bool
valid = walk 0
    where walk count []  = count == 0
          walk count (o:ops)
            | o == Open  = count >= 0 && walk (count+1) ops
            | o == Close = count >= 0 && walk (count-1) ops
            | otherwise  = count >= 0 && walk  count    ops

unlexer :: [Operator] -> String
unlexer = mapMaybe (`lookup` map swap operations)
