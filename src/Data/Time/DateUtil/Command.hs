module Data.Time.DateUtil.Command
  ( Command(..)
  , HolidayCommandType(..)
  , RokuyoCommandType(..)
  ) where

import Data.Time.Calendar

data Command = HelpCommand
             | HolidayCommand { _holidayCommandType :: HolidayCommandType }
             | RokuyoCommand { _rokuyoCommandType :: RokuyoCommandType }
  deriving (Show, Eq)

data HolidayCommandType = HolidayStdOut { _holidayCommandDay :: Maybe Day }
                        | HolidayExitCode { _holidayCommandDay :: Maybe Day }
                        | HolidayHelp
  deriving (Show, Eq)

data RokuyoCommandType = RokuyoStdOut { _rokuyoCommandDay :: Maybe Day }
                       | RokuyoHelp
  deriving (Show, Eq)
