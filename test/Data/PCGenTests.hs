{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.PCGenTests (
    pcGen32Tests,
    pcGen64Tests
    ) where

import Test.Hspec
import Test.QuickCheck

import System.Random

import Data.PCGen

-- -- -- -- --
-- Instances for Arbitrary PCGen values.
-- -- -- -- --

-- | Orphan, but that's alright.
instance Arbitrary PCGen32 where
    arbitrary = do
        w1 <- arbitrary
        w2 <- arbitrary
        return (mkPCGen32 w1 w2)

-- | Orphan, but that's alright.
instance Arbitrary PCGen64 where
    arbitrary = do
        w1 <- arbitrary
        w2 <- arbitrary
        w3 <- arbitrary
        w4 <- arbitrary
        return (mkPCGen64 w1 w2 w3 w4)

-- -- -- -- --
-- PCGen32 tests
-- -- -- -- --

pcGen32Tests :: Spec
pcGen32Tests = describe "PCGen32" $ do
    it "mkPCGen32 always gives odd inc value." $ property $
        \gen32 -> odd (_inc32 gen32)

-- TODO: Test next, genRange, and split for PCGen32

-- -- -- -- --
-- PCGen64 tests
-- -- -- -- --

pcGen64Tests :: Spec
pcGen64Tests = describe "PCGen64" $ do
    it "mkPCGen64 always gives odd inc value." $ property $
        \gen64 -> odd (_inc32._genA $ gen64) && odd (_inc32._genB $ gen64)
    it "mkPCGen64 never gives matching incA and incB." $ property $
        \gen64 -> (_inc32._genA $ gen64) /= (_inc32._genB $ gen64)

-- TODO: Test next, genRange, and split for PCGen64
