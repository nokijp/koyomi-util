module Data.Time.KoyomiUtil.CommandRunner
  ( runCommand
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.KoyomiUtil.Command
import Data.Time.KoyomiUtil.Date
import Data.Time.KoyomiUtil.Internal.Common
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
dayStdOut dateArg = printMultiple (either (const "NOT DETERMINED") id . dayInfo) dateArg

tempoStdOut :: Maybe String -> DateArg -> IO ()
tempoStdOut formatMaybe (DayArg day) = runEither $ tempoString formatMaybe day
tempoStdOut formatMaybe dateArg = printMultiple (either (const "NOT DETERMINED") id . tempoString formatMaybe) dateArg

holidayStdOut :: Bool -> DateArg -> IO ()
holidayStdOut includesWeekends (DayArg day) = runMaybe $ holidayName includesWeekends day
holidayStdOut includesWeekends dateArg = printMultipleMaybe (holidayName includesWeekends) dateArg

holidayExitCode :: Bool -> DateArg -> IO ()
holidayExitCode includesWeekends (DayArg day) = if isJust (holidayName includesWeekends day) then exitSuccess else exitFailure
holidayExitCode _ _ = undefined  -- FIXME

rokuyoStdOut :: DateArg -> IO ()
rokuyoStdOut (DayArg day) = runEither $ rokuyoString day
rokuyoStdOut dateArg = printMultiple (either (const "NOT DETERMINED") id . rokuyoString) dateArg

solarTermStdOut :: DateArg -> IO ()
solarTermStdOut (DayArg day) = runMaybe $ solarTermString day
solarTermStdOut dateArg = printMultipleMaybe solarTermString dateArg

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

printMultiple :: (Day -> String) -> DateArg -> IO ()
printMultiple f dateArg = putStrLn $ showDays f $ toDays dateArg

printMultipleMaybe :: (Day -> Maybe String) -> DateArg -> IO ()
printMultipleMaybe f dateArg = putStrLn $ showDaysMaybe f $ toDays dateArg

exitWithMessage :: String -> IO ()
exitWithMessage message = hPutStrLn stderr message >> exitFailure
