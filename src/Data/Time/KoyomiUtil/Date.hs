module Data.Time.KoyomiUtil.Date
  ( DateArg(..)
  , parseDateArg
  ) where

import Data.Time.Calendar
import Data.List.Split
import Text.Read

data DateArg = DayArg { _dateArgYear :: Integer, _dateArgMonth :: Int, _dateArgDay :: Int }
             | MonthArg { _dateArgYear :: Integer, _dateArgMonth :: Int }
             | YearArg { _dateArgYear :: Integer }
               deriving (Show, Eq)

parseDateArg :: String -> Maybe DateArg
parseDateArg = toDateArg . splitOneOf delimiters

toDateArg :: [String] -> Maybe DateArg
toDateArg [yStr, mStr, dStr] = do
  y <- readMaybe yStr
  m <- readMaybe mStr
  d <- readMaybe dStr
  _ <- fromGregorianValid y m d
  return $ DayArg y m d
toDateArg [yStr, mStr] = do
  y <- readMaybe yStr
  m <- readMaybe mStr
  _ <- fromGregorianValid y m 1
  return $ MonthArg y m
toDateArg [yStr] = do
  y <- readMaybe yStr
  _ <- fromGregorianValid y 1 1
  return $ YearArg y
toDateArg _ = Nothing

delimiters :: [Char]
delimiters = "/- "
