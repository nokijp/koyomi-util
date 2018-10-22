module Data.Time.KoyomiUtil.Internal.RokuyoSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Either
import Data.Time.Calendar
import Data.Time.KoyomiUtil.Internal.Rokuyo
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rokuyoString" $ do
    forM_ [ (fromGregorian 2000 1 1, "大安")
          , (fromGregorian 2000 1 2, "赤口")
          ] $
      \(day, expected) -> it ("should return " ++ show expected ++ " when given " ++ show day) $
        rokuyoString day `shouldBe` Right expected

    forM_ [ (fromGregorian 2033 1 1)
          ] $
      \day -> it ("should return an error when given " ++ show day) $
        rokuyoString day `shouldSatisfy` isLeft
