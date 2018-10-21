module Main where

import Data.Time.DateUtil.CommandRunner
import Data.Time.DateUtil.OptParser

main :: IO ()
main = do
  command <- parseOptIO
  runCommand command
