module Data.Time.KoyomiUtil.OptParserSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.KoyomiUtil.Command
import Data.Time.KoyomiUtil.OptParser
import Options.Applicative
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseOpt" $ do
    forM_ [ ( "no argments are given"
            , []
            )
          , ( "passed an invalid sub-command"
            , ["invalidsubcommand"]
            )
          ] $
      \(ctx, args) -> it ("should fail when " ++ ctx) $
        parseOpt args `shouldSatisfy` isFailure

    describe "kyureki" $ do
      forM_ [ ( ["kyureki"]
              , TempoCommand $ TempoStdOut Nothing Nothing
              )
            , ( ["kyureki", "2000-01-02"]
              , TempoCommand $ TempoStdOut Nothing (Just $ fromGregorian 2000 1 2)
              )
            , ( ["kyureki", "2000-1-2"]
              , TempoCommand $ TempoStdOut Nothing (Just $ fromGregorian 2000 1 2)
              )
            , ( ["kyureki", "2000/01/02"]
              , TempoCommand $ TempoStdOut Nothing (Just $ fromGregorian 2000 1 2)
              )
            , ( ["kyureki", "2000/1/2"]
              , TempoCommand $ TempoStdOut Nothing (Just $ fromGregorian 2000 1 2)
              )
            , ( ["kyureki", "--format", "fmtfmt"]
              , TempoCommand $ TempoStdOut (Just "fmtfmt") Nothing
              )
            , ( ["kyureki", "-f", "fmtfmt"]
              , TempoCommand $ TempoStdOut (Just "fmtfmt") Nothing
              )
            , ( ["kyureki", "--format", "fmtfmt", "2000-01-02"]
              , TempoCommand $ TempoStdOut (Just "fmtfmt") (Just $ fromGregorian 2000 1 2)
              )
            , ( ["kyureki", "-f", "fmtfmt", "2000-01-02"]
              , TempoCommand $ TempoStdOut (Just "fmtfmt") (Just $ fromGregorian 2000 1 2)
              )
            ] $
        \(args, expected) -> it ("should run 'kyureki' with parameters " ++ show expected ++ " when given " ++ show args) $
          unwrapSuccess (parseOpt args) `shouldBe` expected
      forM_ [ ( "given an invalid date"
              , ["kyureki", "x"]
              )
            ] $
        \(ctx, args) -> it ("should fail when " ++ ctx) $
          parseOpt args `shouldSatisfy` isFailure

    describe "holiday" $ do
      forM_ [ ( ["holiday"]
              , HolidayCommand $ HolidayStdOut Nothing False
              )
            , ( ["holiday", "--exit-code"]
              , HolidayCommand $ HolidayExitCode Nothing False
              )
            , ( ["holiday", "2000-01-02"]
              , HolidayCommand $ HolidayStdOut (Just $ fromGregorian 2000 1 2) False
              )
            , ( ["holiday", "--exit-code", "2000-01-02"]
              , HolidayCommand $ HolidayExitCode (Just $ fromGregorian 2000 1 2) False
              )
            , ( ["holiday", "-w"]
              , HolidayCommand $ HolidayStdOut Nothing True
              )
            , ( ["holiday", "--exit-code", "-w"]
              , HolidayCommand $ HolidayExitCode Nothing True
              )
            , ( ["holiday", "-w", "--exit-code"]
              , HolidayCommand $ HolidayExitCode Nothing True
              )
            ] $
        \(args, expected) -> it ("should run 'holiday' with parameters " ++ show expected ++ " when given " ++ show args) $
          unwrapSuccess (parseOpt args) `shouldBe` expected

    describe "rokuyo" $ do
      forM_ [ ( ["rokuyo"]
              , RokuyoCommand $ RokuyoStdOut Nothing
              )
            , ( ["rokuyo", "2000-01-02"]
              , RokuyoCommand $ RokuyoStdOut $ Just $ fromGregorian 2000 1 2
              )
            ] $
        \(args, expected) -> it ("should run 'rokuyo' with parameters " ++ show expected ++ " when given " ++ show args) $
          unwrapSuccess (parseOpt args) `shouldBe` expected

unwrapSuccess :: Show a => ParserResult a -> a
unwrapSuccess (Success a) = a
unwrapSuccess r = error $ show r

isFailure :: ParserResult a -> Bool
isFailure (Failure _) = True
isFailure _ = False
