module Data.Time.KoyomiUtil.Internal.Holiday
  ( holidayName
  ) where

import Control.Applicative
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.JapaneseCalendar

holidayName :: Bool -> Day -> Maybe String
holidayName includesWeekends day =
  let
    publicHolidayName = toJapaneseName <$> holidayType day
    weekendName = dayToWeekendName day
  in if includesWeekends then publicHolidayName <|> weekendName else publicHolidayName

dayToWeekendName :: Day -> Maybe String
dayToWeekendName = weekDateToWeekendName . toWeekDate
  where
    weekDateToWeekendName (_, _, 6) = Just "土曜日"
    weekDateToWeekendName (_, _, 7) = Just "日曜日"
    weekDateToWeekendName _ = Nothing
