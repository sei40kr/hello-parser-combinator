module Lib
  ( anyChar
  , (<|>)
  , char
  , digit
  , letter
  , many
  )
where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                )
import           Data.Char
import           Control.Monad
import           Control.Monad.State

anyChar = StateT anyChar where
  anyChar (x : xs) = Right (x, xs)
  anyChar xs       = Left ("too short", xs)

satisfy f = StateT satisfy where
  satisfy (x : xs) | not $ f x = Left (": " ++ show x, x : xs)
  satisfy xs                   = runStateT anyChar xs

(StateT a) <|> (StateT b) = StateT f where
  f s0 = a s0 <|> b s0   where
    Left (a, s1) <|> _ | s0 /= s1 = Left (a, s1)
    Left (a, _) <|> Left (b, s2)  = Left (b ++ a, s2)
    Left _      <|> b             = b
    a           <|> _             = a

left e = StateT $ \s -> Left (e, s)

char c = satisfy (== c) <|> left ("not char " ++ show c)
digit = satisfy isDigit <|> left "not digit"
letter = satisfy isLetter <|> left "not letter"

many p = ((:) <$> p <*> many p) <|> return []
