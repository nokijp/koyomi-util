module Data.Time.KoyomiUtil.Internal.Common
  ( showDays
  , showDaysMaybe
  ) where

import Data.List
import Data.Maybe
import Data.Time.Calendar

showDays :: (Day -> String) -> [Day] -> String
showDays f days = intercalate "\n" $ (\d -> withDayString d (f d)) <$> days

showDaysMaybe :: (Day -> Maybe String) -> [Day] -> String
showDaysMaybe f days = intercalate "\n" $ mapMaybe (\d -> withDayString d <$> f d) days

withDayString :: Day -> String -> String
withDayString day s = show day ++ ": " ++ s
