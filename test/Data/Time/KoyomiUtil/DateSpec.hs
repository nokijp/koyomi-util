module Data.Time.KoyomiUtil.DateSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.KoyomiUtil.Date
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseDateArg" $ do
    forM_ [ ("2000-01-02", Just $ DayArg $ fromGregorian 2000 1 2)
          , ("2000/01/02", Just $ DayArg $ fromGregorian 2000 1 2)
          , ("2000 01 02", Just $ DayArg $ fromGregorian 2000 1 2)
          , ("2000-1-2", Just $ DayArg $ fromGregorian 2000 1 2)
          , ("2000/1/2", Just $ DayArg $ fromGregorian 2000 1 2)
          , ("2000 1 2", Just $ DayArg $ fromGregorian 2000 1 2)
          , ("2000-01", Just $ MonthArg 2000 1)
          , ("2000/01", Just $ MonthArg 2000 1)
          , ("2000 01", Just $ MonthArg 2000 1)
          , ("2000-1", Just $ MonthArg 2000 1)
          , ("2000/1", Just $ MonthArg 2000 1)
          , ("2000 1", Just $ MonthArg 2000 1)
          , ("2000", Just $ YearArg 2000)
          , ("2000", Just $ YearArg 2000)
          , ("2000", Just $ YearArg 2000)
          , ("", Nothing)
          , ("2000-01-01-01", Nothing)
          , ("--", Nothing)
          , ("-", Nothing)
          , ("a-b-c", Nothing)
          ] $
      \(str, expected) -> it ("should return " ++ show expected ++ " when given " ++ show str) $
        parseDateArg str `shouldBe` expected
