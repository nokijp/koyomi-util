module Data.Time.KoyomiUtil.Internal.SolarTermSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.KoyomiUtil.Internal.SolarTerm
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solarTermString" $ do
    forM_ [ (fromGregorian 2000 1 1, Nothing)
          , (fromGregorian 2000 1 6, Just "小寒")
          ] $
      \(day, expected) -> it ("should return " ++ show expected ++ " when given " ++ show day) $
        solarTermString day `shouldBe` expected
