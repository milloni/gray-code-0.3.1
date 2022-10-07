module Main where

import Control.Monad (unless)
import Test.QuickCheck.Test (verboseCheckWithResult, quickCheckWithResult
                            , Args(..), stdArgs, isSuccess)
import System.Exit (exitFailure)
import System.Environment (getArgs)

import Codec.Binary.Gray_props

main = do
  args <- getArgs
  let qcArgs = stdArgs { maxSuccess = 10000 }
  let testsResult = if (("-v" `elem` args) || ("--verbose" `elem` args))
                    then verboseCheckWithResult
                    else quickCheckWithResult
  result <- testsResult qcArgs all_props
  unless (isSuccess result) $ do
         print result
         exitFailure