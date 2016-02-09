
module Main where

import Test.Hspec

import Data.PCGenTests

main :: IO ()
main = hspec $ do
    pcGen32Tests
    pcGen64Tests
