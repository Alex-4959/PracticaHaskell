module Controller (
  Input(..),
  viToInput
  ) where

import Data.Char (toLower)


data Input = Oest | Sud | Nord | Est

instance Show Input where
  show Oest = "esquerra"
  show Sud  = "avall"
  show Nord = "amunt"
  show Est  = "dreta"

-- maps vi keys to Input
viToInput :: Char -> Maybe Input
viToInput c
  | c' == 'h'   = Just Oest
  | c' == 'j'   = Just Sud
  | c' == 'k'   = Just Nord
  | c' == 'l'   = Just Est
  | otherwise   = Nothing
  where c' = toLower c

