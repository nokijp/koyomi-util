module Data.Time.KoyomiUtil.Internal.Day
  ( dayInfo
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.KoyomiUtil.Internal.Holiday
import Data.Time.KoyomiUtil.Internal.Rokuyo
import Data.Time.KoyomiUtil.Internal.Tempo

dayInfo :: Day -> Either String String
dayInfo day = do
  t <- tempoString Nothing day
  r <- rokuyoString day
  let h = holidayName False day
  return $ unwords $ catMaybes [Just t, Just r, h]
