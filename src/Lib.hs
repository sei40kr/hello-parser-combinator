module Lib
  ( anyChar
  , char
  , digit
  , letter
  )
where

import           Data.Char

anyChar (x : xs) = (x, xs)
satisfy f (x : xs) | f x = (x, xs)

char c = satisfy (== c)
digit = satisfy isDigit
letter = satisfy isLetter
