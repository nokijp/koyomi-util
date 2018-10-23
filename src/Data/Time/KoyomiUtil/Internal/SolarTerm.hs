module Data.Time.KoyomiUtil.Internal.SolarTerm
  ( solarTermString
  ) where

import Data.Time.Calendar
import Data.Time.JapaneseCalendar

solarTermString :: Day -> Maybe String
solarTermString day = toJapaneseName <$> solarTerm jst day
