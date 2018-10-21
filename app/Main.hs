module Main where

import Data.Time.KoyomiUtil.CommandRunner
import Data.Time.KoyomiUtil.OptParser

main :: IO ()
main = do
  command <- parseOptIO
  runCommand command
