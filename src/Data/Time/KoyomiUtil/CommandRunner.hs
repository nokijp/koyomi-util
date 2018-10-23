module Data.Time.KoyomiUtil.CommandRunner
  ( runCommand
  ) where

import Data.Maybe
import Data.Time.Clock
import Data.Time.KoyomiUtil.Command
import Data.Time.KoyomiUtil.Date
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
runCommand (DayCommand (DayStdOut dateMaybe)) = getDate dateMaybe >>= dayStdOut
runCommand (TempoCommand (TempoStdOut formatMaybe dateMaybe)) = getDate dateMaybe >>= tempoStdOut formatMaybe
runCommand (HolidayCommand (HolidayStdOut dateMaybe includesWeekends)) = getDate dateMaybe >>= holidayStdOut includesWeekends
runCommand (HolidayCommand (HolidayExitCode dateMaybe includesWeekends)) = getDate dateMaybe >>= holidayExitCode includesWeekends
runCommand (RokuyoCommand (RokuyoStdOut dateMaybe)) = getDate dateMaybe >>= rokuyoStdOut
runCommand (SolarTermCommand (SolarTermStdOut dateMaybe)) = getDate dateMaybe >>= solarTermStdOut

dayStdOut :: DateArg -> IO ()
dayStdOut (DayArg day) = runEither $ dayInfo day
dayStdOut _ = undefined  -- FIXME

tempoStdOut :: Maybe String -> DateArg -> IO ()
tempoStdOut formatMaybe (DayArg day) = runEither $ tempoString formatMaybe day
tempoStdOut _ _ = undefined  -- FIXME

holidayStdOut :: Bool -> DateArg -> IO ()
holidayStdOut includesWeekends (DayArg day) = runMaybe $ holidayName includesWeekends day
holidayStdOut _ _ = undefined  -- FIXME

holidayExitCode :: Bool -> DateArg -> IO ()
holidayExitCode includesWeekends (DayArg day) = if isJust (holidayName includesWeekends day) then exitSuccess else exitFailure
holidayExitCode _ _ = undefined  -- FIXME

rokuyoStdOut :: DateArg -> IO ()
rokuyoStdOut (DayArg day) = runEither $ rokuyoString day
rokuyoStdOut _ = undefined  -- FIXME

solarTermStdOut :: DateArg -> IO ()
solarTermStdOut (DayArg day) = runMaybe $ solarTermString day
solarTermStdOut _ = undefined  -- FIXME

getDate :: Maybe DateArg -> IO DateArg
getDate dateArg = do
  systemDayUTC <- getCurrentTime
  let systemDayJST = localDay $ utcToLocalTime jst systemDayUTC
  let defaultDateArg = DayArg systemDayJST
  return $ fromMaybe defaultDateArg dateArg

runEither :: Either String String -> IO ()
runEither = either exitWithMessage putStrLn

runMaybe :: Maybe String -> IO ()
runMaybe = maybe (return ()) putStrLn

exitWithMessage :: String -> IO ()
exitWithMessage message = hPutStrLn stderr message >> exitFailure
