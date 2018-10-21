module Data.Time.KoyomiUtil.Command
  ( Command(..)
  , TempoCommandType(..)
  , HolidayCommandType(..)
  , RokuyoCommandType(..)
  ) where

import Data.Time.Calendar

data Command = TempoCommand { _tempoCommandType :: TempoCommandType }
             | HolidayCommand { _holidayCommandType :: HolidayCommandType }
             | RokuyoCommand { _rokuyoCommandType :: RokuyoCommandType }
  deriving (Show, Eq)

data TempoCommandType = TempoStdOut { _tempoCommandFormat :: Maybe String, _tempoCommandDay :: Maybe Day }
  deriving (Show, Eq)

data HolidayCommandType = HolidayStdOut { _holidayCommandDay :: Maybe Day }
                        | HolidayExitCode { _holidayCommandDay :: Maybe Day }
  deriving (Show, Eq)

data RokuyoCommandType = RokuyoStdOut { _rokuyoCommandDay :: Maybe Day }
  deriving (Show, Eq)
