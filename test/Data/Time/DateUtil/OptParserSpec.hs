module Data.Time.DateUtil.OptParserSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.DateUtil.Command
import Data.Time.DateUtil.OptParser
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

    it "should run 'help' when '--help' is passed" $
      unwrapSuccess (parseOpt ["--help"]) `shouldBe` HelpCommand

    describe "holiday" $ do
      forM_ [ ( ["holiday"]
              , HolidayCommand HolidayHelp
              )
            , ( ["holiday", "--exit-code"]
              , HolidayCommand $ HolidayExitCode Nothing
              )
            , ( ["holiday", "2000-01-02"]
              , HolidayCommand $ HolidayStdOut $ Just $ fromGregorian 2000 1 2
              )
            , ( ["holiday", "2000-1-2"]
              , HolidayCommand $ HolidayStdOut $ Just $ fromGregorian 2000 1 2
              )
            , ( ["holiday", "2000/01/02"]
              , HolidayCommand $ HolidayStdOut $ Just $ fromGregorian 2000 1 2
              )
            , ( ["holiday", "2000/1/2"]
              , HolidayCommand $ HolidayStdOut $ Just $ fromGregorian 2000 1 2
              )
            , ( ["holiday", "--exit-code", "2000-01-02"]
              , HolidayCommand $ HolidayExitCode $ Just $ fromGregorian 2000 1 2
              )
            , ( ["holiday", "--help"]
              , HolidayCommand HolidayHelp
              )
            ] $
        \(args, expected) -> it ("should run 'holiday' with parameters " ++ show expected ++ " when given " ++ show args) $
          unwrapSuccess (parseOpt args) `shouldBe` expected
      forM_ [ ( "given an invalid date"
              , ["holiday", "x"]
              )
            , ( "given an invalid option"
              , ["holiday", "-x"]
              )
            , ( "given an invalid option"
              , ["holiday", "--xxx"]
              )
            , ( "given an extra argument"
              , ["holiday", "2000-01-02", "2000-01-02"]
              )
            ] $
        \(ctx, args) -> it ("should fail when " ++ ctx) $
          parseOpt args `shouldSatisfy` isFailure

    describe "rokuyo" $ do
      forM_ [ ( ["rokuyo"]
              , RokuyoCommand $ RokuyoStdOut Nothing
              )
            , ( ["rokuyo", "2000-01-02"]
              , RokuyoCommand $ RokuyoStdOut $ Just $ fromGregorian 2000 1 2
              )
            , ( ["rokuyo", "--help"]
              , RokuyoCommand RokuyoHelp
              )
            ] $
        \(args, expected) -> it ("should run 'rokuyo' with parameters " ++ show expected ++ " when given " ++ show args) $
          unwrapSuccess (parseOpt args) `shouldBe` expected
      forM_ [ ( "given an invalid date"
              , ["rokuyo", "x"]
              )
            , ( "given an invalid option"
              , ["rokuyo", "-x"]
              )
            , ( "given an invalid option"
              , ["rokuyo", "--xxx"]
              )
            , ( "given an extra argument"
              , ["rokuyo", "2000-01-02", "2000-01-02"]
              )
            ] $
        \(ctx, args) -> it ("should fail when " ++ ctx) $
          parseOpt args `shouldSatisfy` isFailure

unwrapSuccess :: Show a => ParserResult a -> a
unwrapSuccess (Success a) = a
unwrapSuccess r = error $ show r

isFailure :: ParserResult a -> Bool
isFailure (Failure _) = True
isFailure _ = False
