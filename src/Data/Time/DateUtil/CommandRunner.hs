module Data.Time.DateUtil.CommandRunner
  ( runCommand
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.DateUtil.Command
import Data.Time.JapaneseCalendar
import Data.Time.LocalTime
import System.Exit

runCommand :: Command -> IO ()
runCommand (HolidayCommand (HolidayStdOut dayMaybe)) = getDay dayMaybe >>= holidayStdOut
runCommand (HolidayCommand (HolidayExitCode dayMaybe)) = getDay dayMaybe >>= holidayExitCode
runCommand (RokuyoCommand (RokuyoStdOut dayMaybe)) = getDay dayMaybe >>= rokuyoStdOut

holidayStdOut :: Day -> IO ()
holidayStdOut = maybe (return ()) (putStrLn . toJapaneseName) . holidayType

holidayExitCode :: Day -> IO ()
holidayExitCode = maybe exitFailure (const exitSuccess) . holidayType

rokuyoStdOut :: Day -> IO ()
rokuyoStdOut = maybe exitFailure (putStrLn . toJapaneseName) . rokuyo jst

getDay :: Maybe Day -> IO Day
getDay day = do
  systemDayUTC <- getCurrentTime
  let systemDayJST = localDay $ utcToLocalTime jst systemDayUTC
  return $ fromMaybe systemDayJST day
