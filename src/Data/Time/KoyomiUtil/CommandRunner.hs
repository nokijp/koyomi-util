module Data.Time.KoyomiUtil.CommandRunner
  ( runCommand
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.KoyomiUtil.Command
import Data.Time.KoyomiUtil.Internal.Day
import Data.Time.KoyomiUtil.Internal.Holiday
import Data.Time.KoyomiUtil.Internal.Rokuyo
import Data.Time.KoyomiUtil.Internal.SolarTerm
import Data.Time.KoyomiUtil.Internal.Tempo
import Data.Time.JapaneseCalendar
import Data.Time.LocalTime
import System.Exit
import System.IO

runCommand :: Command -> IO ()
runCommand (DayCommand (DayStdOut dayMaybe)) = getDay dayMaybe >>= dayStdOut
runCommand (TempoCommand (TempoStdOut formatMaybe dayMaybe)) = getDay dayMaybe >>= tempoStdOut formatMaybe
runCommand (HolidayCommand (HolidayStdOut dayMaybe includesWeekends)) = getDay dayMaybe >>= holidayStdOut includesWeekends
runCommand (HolidayCommand (HolidayExitCode dayMaybe includesWeekends)) = getDay dayMaybe >>= holidayExitCode includesWeekends
runCommand (RokuyoCommand (RokuyoStdOut dayMaybe)) = getDay dayMaybe >>= rokuyoStdOut
runCommand (SolarTermCommand (SolarTermStdOut dayMaybe)) = getDay dayMaybe >>= solarTermStdOut

dayStdOut :: Day -> IO ()
dayStdOut = runEither . dayInfo

tempoStdOut :: Maybe String -> Day -> IO ()
tempoStdOut formatMaybe = runEither . tempoString formatMaybe

holidayStdOut :: Bool -> Day -> IO ()
holidayStdOut includesWeekends = runMaybe . holidayName includesWeekends

holidayExitCode :: Bool -> Day -> IO ()
holidayExitCode includesWeekends day = if isJust (holidayName includesWeekends day) then exitSuccess else exitFailure

rokuyoStdOut :: Day -> IO ()
rokuyoStdOut = runEither . rokuyoString

solarTermStdOut :: Day -> IO ()
solarTermStdOut = runMaybe . solarTermString

getDay :: Maybe Day -> IO Day
getDay day = do
  systemDayUTC <- getCurrentTime
  let systemDayJST = localDay $ utcToLocalTime jst systemDayUTC
  return $ fromMaybe systemDayJST day

runEither :: Either String String -> IO ()
runEither = either exitWithMessage putStrLn

runMaybe :: Maybe String -> IO ()
runMaybe = maybe (return ()) putStrLn

exitWithMessage :: String -> IO ()
exitWithMessage message = hPutStrLn stderr message >> exitFailure
