module Data.Time.KoyomiUtil.Command
  ( Command(..)
  , DayCommandType(..)
  , TempoCommandType(..)
  , HolidayCommandType(..)
  , RokuyoCommandType(..)
  , SolarTermCommandType(..)
  ) where

import Data.Time.Calendar

data Command = DayCommand { _dayCommandType :: DayCommandType }
             | TempoCommand { _tempoCommandType :: TempoCommandType }
             | HolidayCommand { _holidayCommandType :: HolidayCommandType }
             | RokuyoCommand { _rokuyoCommandType :: RokuyoCommandType }
             | SolarTermCommand { _solarTermCommandType :: SolarTermCommandType }
  deriving (Show, Eq)

data DayCommandType = DayStdOut { _dayCommandDay :: Maybe Day } deriving (Show, Eq)

data TempoCommandType = TempoStdOut { _tempoCommandFormat :: Maybe String, _tempoCommandDay :: Maybe Day } deriving (Show, Eq)

data HolidayCommandType = HolidayStdOut { _holidayCommandDay :: Maybe Day, _includesWeekends :: Bool }
                        | HolidayExitCode { _holidayCommandDay :: Maybe Day, _includesWeekends :: Bool }
                          deriving (Show, Eq)

data RokuyoCommandType = RokuyoStdOut { _rokuyoCommandDay :: Maybe Day } deriving (Show, Eq)

data SolarTermCommandType = SolarTermStdOut { _solarTermCommandDay :: Maybe Day } deriving (Show, Eq)
