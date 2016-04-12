
module Main where

import Test.Hspec

import Data.PCGenTests
import Control.RNGTests

main :: IO ()
main = hspec $ do
    pcGen32Tests
    pcGen64Tests
    rngTests
