module Data.Time.KoyomiUtil.OptParser
  ( parseOpt
  , parseOptIO
  ) where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.KoyomiUtil.Command
import Options.Applicative

parseOpt :: [String] -> ParserResult Command
parseOpt = execParserPure pref parserInfo

pref :: ParserPrefs
pref = defaultPrefs { prefShowHelpOnError = True
                    , prefShowHelpOnEmpty = True
                    }

parseOptIO :: IO Command
parseOptIO = execParser parserInfo

parserInfo :: ParserInfo Command
parserInfo = info (parser <**> helper) $ progDesc "A tool for dealing with the Japanese calendar" <> fullDesc

parser :: Parser Command
parser = subparser (  command "day" (info ((DayCommand <$> dayOptParser) <**> helper)
                                          (progDesc "Show Japanese calendar" <> fullDesc)
                                    )
                   <> command "kyureki" (info ((TempoCommand <$> tempoOptParser) <**> helper)
                                              (progDesc "Show Japanese Kyureki" <> fullDesc)
                                        )
                   <> command "holiday" (info ((HolidayCommand <$> holidayOptParser) <**> helper)
                                              (progDesc "Show name of holiday if today (or the specified day) is holiday" <> fullDesc)
                                        )
                   <> command "rokuyo" (info ((RokuyoCommand <$> rokuyoOptParser) <**> helper)
                                             (progDesc "Show name of Rokuyo" <> fullDesc)
                                       )
                   <> command "sekki" (info ((SolarTermCommand <$> solarTermOptParser) <**> helper)
                                            (progDesc "Show name of the twenty-four solar term" <> fullDesc)
                                      )
                   )
         <|> pure (DayCommand $ DayStdOut Nothing)

dayOptParser :: Parser DayCommandType
dayOptParser = DayStdOut <$> dateParser

tempoOptParser :: Parser TempoCommandType
tempoOptParser = TempoStdOut <$> optional (strOption (long "format" <> short 'f' <> help helpString))
                             <*> dateParser
  where
    helpString = "Set a format string to print (default: \"%y年%M月%d日\")"

holidayOptParser :: Parser HolidayCommandType
holidayOptParser = (\c -> if c then HolidayExitCode else HolidayStdOut) <$> switch (long "exit-code" <> help helpString)
                                                                        <*> dateParser
                                                                        <*> switch (short 'w' <> help "Include weekend days as holidays")
  where
    helpString = "Exit with 0 if the specified day is a holiday, otherwise exit with 1"

rokuyoOptParser :: Parser RokuyoCommandType
rokuyoOptParser = RokuyoStdOut <$> dateParser

solarTermOptParser :: Parser SolarTermCommandType
solarTermOptParser = SolarTermStdOut <$> dateParser

dateParser :: Parser (Maybe Day)
dateParser = optional (argument (eitherReader parseDate) (metavar "DATE"))
  where
    parseDate :: String -> Either String Day
    parseDate s = maybe (Left $ "Invalid date format: " ++ s) Right $ parseResult s
    parseResult :: String -> Maybe Day
    parseResult s =   parseDateMaybe "%Y-%-m-%-d" s
                  <|> parseDateMaybe "%Y/%-m/%-d" s
    parseDateMaybe :: String -> String -> Maybe Day
    parseDateMaybe = parseTimeM True defaultTimeLocale
