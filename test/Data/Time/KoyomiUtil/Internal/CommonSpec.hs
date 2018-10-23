module Data.Time.KoyomiUtil.Internal.CommonSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.KoyomiUtil.Internal.Common
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "showDays" $ do
    let f day = let (_, _, d) = toGregorian day in show d
    forM_ [ ([], "")
          , ([fromGregorian 2000 1 2], "2000-01-02: 2")
          , ([fromGregorian 2000 1 1 .. fromGregorian 2000 1 3], "2000-01-01: 1\n2000-01-02: 2\n2000-01-03: 3")
          ] $
      \(days, expected) -> it ("should return " ++ show expected ++ " when given " ++ show days) $
        showDays f days `shouldBe` expected

  describe "showDaysMaybe" $ do
    let f day = let (_, _, d) = toGregorian day in if d `mod` 2 == 0 then Just $ show d else Nothing
    forM_ [ ([], "")
          , ([fromGregorian 2000 1 1], "")
          , ([fromGregorian 2000 1 2], "2000-01-02: 2")
          , ([fromGregorian 2000 1 1 .. fromGregorian 2000 1 4], "2000-01-02: 2\n2000-01-04: 4")
          ] $
      \(days, expected) -> it ("should return " ++ show expected ++ " when given " ++ show days) $
        showDaysMaybe f days `shouldBe` expected
