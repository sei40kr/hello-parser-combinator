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

anyChar = StateT anyChar where
  anyChar (x : xs) = Right (x, xs)
  anyChar _        = Left "too short"

satisfy f = StateT satisfy where
  satisfy (x : xs) | not $ f x = Left $ ": " ++ show x
  satisfy xs                   = runStateT anyChar xs

(StateT a) <|> (StateT b) = StateT $ \s -> a s <|> b s where
  Left a <|> Left b = Left $ b ++ a
  Left _ <|> b      = b
  a      <|> _      = a

left = lift . Left

char c = satisfy (== c) <|> left ("not char " ++ show c)
digit = satisfy isDigit <|> left "not digit"
letter = satisfy isLetter <|> left "not letter"
