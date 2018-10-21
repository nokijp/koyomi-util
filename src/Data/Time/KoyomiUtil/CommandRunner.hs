module Data.Time.KoyomiUtil.CommandRunner
  ( runCommand
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.KoyomiUtil.Command
import Data.Time.JapaneseCalendar
import Data.Time.LocalTime
import System.Exit
import System.IO

runCommand :: Command -> IO ()
runCommand (TempoCommand (TempoStdOut formatMaybe dayMaybe)) = getDay dayMaybe >>= tempoStdOut formatMaybe
runCommand (HolidayCommand (HolidayStdOut dayMaybe)) = getDay dayMaybe >>= holidayStdOut
runCommand (HolidayCommand (HolidayExitCode dayMaybe)) = getDay dayMaybe >>= holidayExitCode
runCommand (RokuyoCommand (RokuyoStdOut dayMaybe)) = getDay dayMaybe >>= rokuyoStdOut

tempoStdOut :: Maybe String -> Day -> IO ()
tempoStdOut formatMaybe day =
  let
    format = fromMaybe "%y年%M月%d日" formatMaybe
    stringEither = do
      td <- maybe (Left "Kyureki calculation failed") Right $ tempoDate jst day
      formatTempoDate format td
  in either exitWithMessage putStrLn stringEither

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

exitWithMessage :: String -> IO ()
exitWithMessage message = hPutStrLn stderr message >> exitFailure
