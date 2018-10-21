module Data.Time.KoyomiUtil.Internal.TempoSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Either
import Data.Time.Calendar
import Data.Time.KoyomiUtil.Internal.Tempo
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tempoString" $ do
    forM_ [ (Nothing, fromGregorian 2000 1 1, "1999年11月25日")
          , (Just "%y", fromGregorian 2000 1 1, "1999")
          ] $
      \(formatMaybe, day, expected) -> it ("should return " ++ show expected ++ " when given " ++ show (formatMaybe, day)) $
        tempoString formatMaybe day `shouldBe` Right expected

    forM_ [ (Nothing, fromGregorian 2033 1 1)
          , (Just "%x", fromGregorian 2000 1 1)
          ] $
      \(formatMaybe, day) -> it ("should return an error when given " ++ show (formatMaybe, day)) $
        tempoString formatMaybe day `shouldSatisfy` isLeft
