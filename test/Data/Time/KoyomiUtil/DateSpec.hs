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

  describe "toDays" $ do
    forM_ [ (DayArg $ fromGregorian 2000 1 2, [fromGregorian 2000 1 2])
          , (MonthArg 2000 1, [fromGregorian 2000 1 1 .. fromGregorian 2000 1 31])
          , (MonthArg 2000 2, [fromGregorian 2000 2 1 .. fromGregorian 2000 2 29])
          , (MonthArg 2001 2, [fromGregorian 2001 2 1 .. fromGregorian 2001 2 28])
          , (MonthArg 2000 12, [fromGregorian 2000 12 1 .. fromGregorian 2000 12 31])
          , (YearArg 2000, [fromGregorian 2000 1 1 .. fromGregorian 2000 12 31])
          ] $
      \(dateArg, expected) -> it ("should return " ++ show expected ++ " when given " ++ show dateArg) $
        toDays dateArg `shouldBe` expected
