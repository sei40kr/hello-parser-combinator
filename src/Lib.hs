module Lib
  ( anyChar
  , satisfy
  )
where

anyChar (x : xs) = (x, xs)

satisfy f (x : xs) | f x = (x, xs)
