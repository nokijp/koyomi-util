module Data.Time.KoyomiUtil.Internal.DaySpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Either
import Data.Time.Calendar
import Data.Time.KoyomiUtil.Internal.Day
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dayInfo" $ do
    forM_ [ (fromGregorian 2000 1 1, "1999年11月25日 大安 元日")
          , (fromGregorian 2000 1 2, "1999年11月26日 赤口")
          ] $
      \(day, expected) -> it ("should return " ++ show expected ++ " when given " ++ show day) $
        dayInfo day `shouldBe` Right expected

    forM_ [ (fromGregorian 2033 1 1)
          ] $
      \day -> it ("should return an error when given " ++ show day) $
        dayInfo day `shouldSatisfy` isLeft
