module Data.Time.DateUtil.OptParser
  ( parseOpt
  , parseOptIO
  ) where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.DateUtil.Command
import Options.Applicative

parseOpt :: [String] -> ParserResult Command
parseOpt = execParserPure defaultPrefs parserInfo

parseOptIO :: IO Command
parseOptIO = execParser parserInfo

parserInfo :: ParserInfo Command
parserInfo = info parser $ progDesc "a tool for dealing with the Japanese calendar" <> fullDesc

parser :: Parser Command
parser =   subparser (  command "holiday" (info (HolidayCommand <$> holidayOptParser)
                                                (progDesc "show the name of holiday if today (or the specified day) is holiday" <> fullDesc)
                                          )
                     <> command "rokuyo" (info (RokuyoCommand <$> rokuyoOptParser)
                                               (progDesc "show the name of Rokuyo" <> fullDesc)
                                         )
                     )
       <|> defaultOptParser

defaultOptParser :: Parser Command
defaultOptParser = HelpCommand <$ flag' () (long "help" <> help "show this help")

holidayOptParser :: Parser HolidayCommandType
holidayOptParser =   (HolidayHelp <$ switch (long "help" <> help "show this help"))
                 <|> (HolidayStdOut <$> dateParser)
                 <|> (HolidayExitCode <$> (  flag' () (long "exit-code" <> help "exit with 0 if the specified day is a holiday, otherwise exit with 1")
                                          *> dateParser
                                          )
                     )

rokuyoOptParser :: Parser RokuyoCommandType
rokuyoOptParser =   (RokuyoHelp <$ flag' () (long "help" <> help "show this help"))
                <|> (RokuyoStdOut <$> dateParser)

dateParser :: Parser (Maybe Day)
dateParser = optional (argument (eitherReader parseDate) (metavar "DATE"))
  where
    parseDate :: String -> Either String Day
    parseDate s = maybe (Left $ "invalid date format: " ++ s) Right $ parseResult s
    parseResult :: String -> Maybe Day
    parseResult s =   parseDateMaybe "%Y-%-m-%-d" s
                  <|> parseDateMaybe "%Y/%-m/%-d" s
    parseDateMaybe :: String -> String -> Maybe Day
    parseDateMaybe = parseTimeM True defaultTimeLocale
