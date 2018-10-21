module Data.Time.KoyomiUtil.Internal.Tempo
  ( tempoString
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.JapaneseCalendar

tempoString :: Maybe String -> Day -> Either String String
tempoString formatMaybe day =
  let
    format = fromMaybe "%y年%M月%d日" formatMaybe
    stringEither = do
      td <- maybe (Left "Kyureki calculation failed") Right $ tempoDate jst day
      formatTempoDate format td
  in stringEither
