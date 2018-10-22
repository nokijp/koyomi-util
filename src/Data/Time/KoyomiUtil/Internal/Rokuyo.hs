module Data.Time.KoyomiUtil.Internal.Rokuyo
  ( rokuyoString
  ) where

import Data.Time.Calendar
import Data.Time.JapaneseCalendar

rokuyoString :: Day -> Either String String
rokuyoString = maybe (Left "Ryokuyo calculation failed") (Right . toJapaneseName) . rokuyo jst
