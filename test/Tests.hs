
module Main where

import Test.Hspec

import Data.PCGenTests
import Control.RNGTests
import Util.PPFOVTests
--import Util.PPFOVPrivateTests

main :: IO ()
main = hspec $ do
    pcGen32Tests
    pcGen64Tests
    rngTests
    ppfovTests
    --ppfovPrivateTests
