{-# LANGUAGE CPP #-}
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

propGenRange :: (RandomGen g) => g -> g -> Bool
propGenRange = \rgenA rgenB -> genRange rgenA == genRange rgenB

propNextInRange :: (RandomGen g) => g -> Bool
propNextInRange = \randomGen -> let
        (i,g) = next randomGen
        (low, high) = genRange g
        in i >= low && i <= high

propSplit :: (RandomGen g, Eq g) => g -> Bool
propSplit = \randomGen -> let
    (genL, genR) = split randomGen
    in (genL /= randomGen) && (genR /= randomGen) && (genL /= genR)

-- -- -- -- --
-- PCGen32 tests
-- -- -- -- --

pcGen32Tests :: Spec
#ifdef SixtyFourBit
pcGen32Tests = describe "PCGen32 (64-bit)" $ do
#else
pcGen32Tests = describe "PCGen32 (32-bit)" $ do
#endif
    it "mkPCGen32 always gives odd inc value." $ property $
#ifdef SixtyFourBit
        \gen32 -> odd (read (words (show (gen32::PCGen32)) !! 2) :: Word)
#else
        \gen32 -> odd ((read [last.init $ (show (gen32::PCGen32))])::Int)
#endif
    it "read always reads back exactly what show generated" $ property $
        \gen32 -> gen32 == (read (show (gen32::PCGen32)))
    it "read can parse arbitrary semi-valid strings" $ property $
#ifdef SixtyFourBit
        \w1 w2 -> Prelude.seq (read ("PCGen32 "++show (w1::Word)++" "++show (w2::Word))::PCGen32) True
#else
        \w1 w2 -> Prelude.seq (read ("PCGen32 {_state32 = "++show (w1::Word)++", _inc32 = "++show (w2::Word)++"}")::PCGen32) True
#endif
    it "genRange ignores the input generator given" $ property $
        \gen32A gen32B -> propGenRange (gen32A::PCGen32) (gen32B::PCGen32)
    it "next is always within the bounds given by genRange" $ property $
        \gen32 -> propNextInRange (gen32::PCGen32)
    it "split results are distinct from both the input and each other" $ property $
        \gen32 -> propSplit (gen32::PCGen32)

-- -- -- -- --
-- PCGen64 tests
-- -- -- -- --

pcGen64Tests :: Spec
#ifdef SixtyFourBit
pcGen64Tests = describe "PCGen64 (64-bit)" $ do
#else
pcGen64Tests = describe "PCGen64 (32-bit)" $ do
#endif
    it "mkPCGen64 always gives odd inc value." $ property $
#ifdef SixtyFourBit
        \gen64 -> odd (read (words (show (gen64::PCGen64)) !! 2) :: Word)
#else
        \gen64 -> odd ((read [last.init.init $ (show (gen64::PCGen64))])::Int)
#endif
    it "read always reads back exactly what show generated" $ property $
        \gen64 -> gen64 == (read (show (gen64::PCGen64)))
    it "read can parse arbitrary semi-valid strings" $ property $
#ifdef SixtyFourBit
        \w1 w2 w3 w4 -> Prelude.seq (read ("PCGen64 "++show (w1::Word)++" "++show (w2::Word)++" "++show (w3::Word)++" "++show (w4::Word))::PCGen64) True
#else
        \w1 w2 w3 w4 -> Prelude.seq (read ("PCGen64 {_genA = PCGen32 {_state32 = "++
            show (w1::Word)++", _inc32 = "++
            show (w2::Word)++"}, _genB = PCGen32 {_state32 = "++
            show (w3::Word)++", _inc32 = "++
            show (w4::Word)++"}}")::PCGen64) True
#endif
    it "genRange ignores the input generator given" $ property $
        \gen64A gen64B -> propGenRange (gen64A::PCGen64) (gen64B::PCGen64)
    it "next is always within the bounds given by genRange" $ property $
        \gen64 -> propNextInRange (gen64::PCGen64)
    it "split results are distinct from both the input and each other" $ property $
        \gen64 -> propSplit (gen64::PCGen64)
