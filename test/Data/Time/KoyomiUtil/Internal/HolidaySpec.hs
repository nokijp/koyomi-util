module Data.Time.KoyomiUtil.Internal.HolidaySpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.KoyomiUtil.Internal.Holiday
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "holidayName" $ do
    forM_ [ (False, fromGregorian 2000 1 1, Just "元日")
          , (False, fromGregorian 2000 1 2, Nothing)
          , (False, fromGregorian 2000 1 3, Nothing)
          , (False, fromGregorian 2000 1 4, Nothing)
          , (False, fromGregorian 2000 1 5, Nothing)
          , (False, fromGregorian 2000 1 6, Nothing)
          , (False, fromGregorian 2000 1 7, Nothing)
          , (False, fromGregorian 2000 1 8, Nothing)
          , (True, fromGregorian 2000 1 1, Just "元日")
          , (True, fromGregorian 2000 1 2, Just "日曜日")
          , (True, fromGregorian 2000 1 3, Nothing)
          , (True, fromGregorian 2000 1 4, Nothing)
          , (True, fromGregorian 2000 1 5, Nothing)
          , (True, fromGregorian 2000 1 6, Nothing)
          , (True, fromGregorian 2000 1 7, Nothing)
          , (True, fromGregorian 2000 1 8, Just "土曜日")
          ] $
      \(includesWeekends, day, expected) -> it ("should return " ++ show expected ++ " when given " ++ show (includesWeekends, day)) $
        holidayName includesWeekends day `shouldBe` expected
