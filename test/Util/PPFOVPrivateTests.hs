{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MagicHash, BangPatterns #-}

{-| This tests the Util.PPFOV module's private functions.
-}
module Util.PPFOVPrivateTests (
    ppfovPrivateTests
    ) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as S
import GHC.Exts
import Control.Exception

import Data.Location
import Util.PPFOV
import Util.PPFOVTests
import Control.RNG
import Control.RNGTests

instance Arbitrary SightLine where
    arbitrary = do
        !(I# xi) <- arbitrary
        !(I# xf) <- arbitrary
        !(I# yi) <- arbitrary
        !(I# yf) <- arbitrary
        return (SightLine xi xf yi yf)

-- -- -- -- --
-- PPFOV Private Tests
-- -- -- -- --

ppfovPrivateTests :: Spec
ppfovPrivateTests = describe "PPFOV Private Functions" $ do
    it "add works" $ property $
        \list' seed -> let list = list' :: [Int]
                           !(I# index) = (doRand (rollOne (length list)) seed) -1
                           newList = add index 0 list
                           in if null list
                            then evaluate newList `shouldThrow` anyException 
                            else evaluate (length list + 1 == length newList) `shouldReturn` True
    it "update works" $ property $
        \list' seed -> let list = list' :: [Int]
                           !(I# index) = (doRand (rollOne (length list)) seed) -1
                           newList = update index seed list
                           in if null list
                            then evaluate newList `shouldThrow` anyException 
                            else evaluate ((newList #! index) == seed) `shouldReturn` True
    it "update works" $ property $
        \list' seed -> let list = list' :: [Int]
                           !(I# index) = (doRand (rollOne (length list)) seed) -1
                           newList = update index seed list
                           in if null list
                            then evaluate newList `shouldThrow` anyException 
                            else evaluate ((newList #! index) == seed) `shouldReturn` True
    it "upIndexing works" $ property $
        \list' seed -> let list = list' :: [Int]
                           !(I# index) = (doRand (rollOne (length list)) seed) -1
                           in if null list
                            then evaluate (list #! index)  `shouldThrow` anyException 
                            else evaluate ((list #! index) == (list !! (I# index))) `shouldReturn` True
    it "SightLine self-collinear" $ property $
        \sightLine -> isCollinear sightLine (getXInitial sightLine) (getYInitial sightLine)
            && isCollinear sightLine (getXFinal sightLine) (getYFinal sightLine)
    it "ubLen computes lengths correctly" $ property $
        \list' -> let list = list' :: [Int] in (I# (ubLen list)) == (length list :: Int)
