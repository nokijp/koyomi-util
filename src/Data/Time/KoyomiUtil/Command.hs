module Data.Time.KoyomiUtil.Command
  ( Command(..)
  , DayCommandType(..)
  , TempoCommandType(..)
  , HolidayCommandType(..)
  , RokuyoCommandType(..)
  , SolarTermCommandType(..)
  ) where

import Data.Time.KoyomiUtil.Date

data Command = DayCommand { _dayCommandType :: DayCommandType }
             | TempoCommand { _tempoCommandType :: TempoCommandType }
             | HolidayCommand { _holidayCommandType :: HolidayCommandType }
             | RokuyoCommand { _rokuyoCommandType :: RokuyoCommandType }
             | SolarTermCommand { _solarTermCommandType :: SolarTermCommandType }
               deriving (Show, Eq)

data DayCommandType = DayStdOut { _dayCommandDate :: Maybe DateArg } deriving (Show, Eq)

data TempoCommandType = TempoStdOut { _tempoCommandFormat :: Maybe String, _tempoCommandDate :: Maybe DateArg } deriving (Show, Eq)

data HolidayCommandType = HolidayStdOut { _holidayCommandDate :: Maybe DateArg, _includesWeekends :: Bool }
                        | HolidayExitCode { _holidayCommandDate :: Maybe DateArg, _includesWeekends :: Bool }
                          deriving (Show, Eq)

data RokuyoCommandType = RokuyoStdOut { _rokuyoCommandDate :: Maybe DateArg } deriving (Show, Eq)

data SolarTermCommandType = SolarTermStdOut { _solarTermCommandDate :: Maybe DateArg } deriving (Show, Eq)
