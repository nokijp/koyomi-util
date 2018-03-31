module Lib
  ( someFunc
  ) where

import Data.Time.Calendar
import Data.Time.JapaneseCalendar.Holiday

someFunc :: IO ()
someFunc = do
  let day = fromGregorian 2018 3 31
  print $ holidayType day
