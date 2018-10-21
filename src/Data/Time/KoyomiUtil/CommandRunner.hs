module Data.Time.KoyomiUtil.CommandRunner
  ( runCommand
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.KoyomiUtil.Command
import Data.Time.KoyomiUtil.Internal.Holiday
import Data.Time.KoyomiUtil.Internal.Tempo
import Data.Time.JapaneseCalendar
import Data.Time.LocalTime
import System.Exit
import System.IO

runCommand :: Command -> IO ()
runCommand (TempoCommand (TempoStdOut formatMaybe dayMaybe)) = getDay dayMaybe >>= tempoStdOut formatMaybe
runCommand (HolidayCommand (HolidayStdOut dayMaybe includesWeekends)) = getDay dayMaybe >>= holidayStdOut includesWeekends
runCommand (HolidayCommand (HolidayExitCode dayMaybe includesWeekends)) = getDay dayMaybe >>= holidayExitCode includesWeekends
runCommand (RokuyoCommand (RokuyoStdOut dayMaybe)) = getDay dayMaybe >>= rokuyoStdOut

tempoStdOut :: Maybe String -> Day -> IO ()
tempoStdOut formatMaybe day = either exitWithMessage putStrLn $ tempoString formatMaybe day

holidayStdOut :: Bool -> Day -> IO ()
holidayStdOut includesWeekends day = maybe (return ()) putStrLn $ holidayName includesWeekends day

holidayExitCode :: Bool -> Day -> IO ()
holidayExitCode includesWeekends day = if isJust (holidayName includesWeekends day) then exitSuccess else exitFailure

rokuyoStdOut :: Day -> IO ()
rokuyoStdOut = maybe exitFailure (putStrLn . toJapaneseName) . rokuyo jst

getDay :: Maybe Day -> IO Day
getDay day = do
  systemDayUTC <- getCurrentTime
  let systemDayJST = localDay $ utcToLocalTime jst systemDayUTC
  return $ fromMaybe systemDayJST day

exitWithMessage :: String -> IO ()
exitWithMessage message = hPutStrLn stderr message >> exitFailure
