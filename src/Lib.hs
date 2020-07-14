module Lib
  ( anyChar
  , char
  , digit
  , letter
  )
where

import           Data.Char
import           Control.Monad
import           Control.Monad.State

anyChar :: State String Char
anyChar = state anyChar where anyChar (x : xs) = (x, xs)

satisfy :: (Char -> Bool) -> State String Char
satisfy f = state satisfy where satisfy (x : xs) | f x = (x, xs)

char c = satisfy (== c)
digit = satisfy isDigit
letter = satisfy isLetter
