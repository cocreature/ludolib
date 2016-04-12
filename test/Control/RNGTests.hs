
{-| This tests the Control.RNG module. For many tests, the upper bounds of input
are limited to prevent overflows, and also to keep the test from taking too long
(eg: rolling 1,000,000d3 won't ever overflow an Int, but it will take annoyingly
long to perform).
-}
module Control.RNGTests (
    rngTests
    ) where

import Test.Hspec
import Test.QuickCheck

import Control.RNG
import Control.Monad.Random
import Data.Maybe
import Control.Monad
import Control.Exception
import System.Random

import Data.PCGen

doRand :: Rand PCGen a -> Int -> a
doRand rand seed = fst $ runRand rand (mkPCGen seed)

--doRand :: Rand StdGen a -> Int -> a
--doRand rand seed = fst $ runRand rand (mkStdGen seed)

-- -- -- -- --
-- RNG Tests
-- -- -- -- --

rngTests :: Spec
rngTests = describe "Control.RNG" $ do
    it "rollOne is always in 1 to x, or 0 if x <= 0." $ property $
        \n seed -> let out = doRand (rollOne n) seed :: Int in case compare n 0 of
            GT -> out >= 1 && out <= n
            _ -> out == 0
    it "rollxdy is always in x to x*y, or 0 if x<=0 or y<=0." $ property $
        \x' y' seed -> let x = (min x' 10000)
                           y = (min y' 10000)
                           out = doRand (rollxdy x y) seed :: Int in case (compare x 0, compare y 0) of
            (GT, GT) -> out >= x && out <= x*y
            (_,_) -> out == 0
    it "rollxdy' has correct result count, and all results in range." $ property $
        \x' y' seed -> let x = (min x' 10000)
                           y = (min y' 10000)
                           out = doRand (rollxdy' x y) seed :: [Int] in case (compare x 0, compare y 0) of
            (GT, GT) -> length out == x && all (\i -> i>=1 && i<=y) out
            (_, _) -> all (==0) out
    it "rollPool gives sensible numbers of hits." $ property $
        \c' s' t' seed -> let c = min c' 10000
                              s = min s' 10000
                              t = min t' 10000
                              out = doRand (rollPool c s t) seed :: Int in case (compare c 0, compare s 0) of
            (GT, GT) -> if t > s 
                then out == 0 -- if the TN is bigger than Sides, we'll always get 0 hits.
                else out >= 0 && out <= c -- Here we'll get some random amount of hits.
            (_, _) -> out >= 0 -- nonsense inputs will always still give some non-negative value
    it "rollChance gives good results." $ property $
        \c' seed -> let c = c' :: Int
                        out = doRand (rollChance c) seed in if c > 99
            then out == True -- more than 99 must always pass
            else if c < 1
                then out == False -- less than 1 it must always fail
                else True -- within 1 to 99 might or might not pass, we don't care which way it goes.
    it "rollChanceIn gives good results." $ property $
        \c' i seed -> let c = c' :: Int
                          out = doRand (rollChanceIn c i) seed in case compare i 0 of
            GT -> if c > i
                then out == True -- c more than i must always pass
                else if c < 1
                    then out == False -- c less than 1 with non-zero range must always fail
                    else True -- any other situation we don't care.
            _ -> True -- If i is some nonsensical value we don't care.
    it "pickRandom gives correct results." $ property $
        \foldable' seed -> let foldable = foldable' :: [Int]
                               out = doRand (pickRandom foldable) seed in if null foldable
            then isNothing out
            else isJust out
    it "pickRandomNonempty gives correct results." $ property $
        \foldable' seed -> let foldable = foldable' :: [Int]
                               out = doRand (pickRandomNonempty foldable) seed in if null foldable
            then evaluate out `shouldThrow` anyException
            else void (evaluate out) `shouldReturn` ()
    it "rollExplode is always positive with positive input (0 otherwise)." $ property $
        \i' seed -> let i = min i' 10000
                        out = doRand (rollExplode i) seed :: Int in case compare i 0 of
            GT -> out > 0
            _ -> out == 0
    it "rollStep4 is always positive with positive input (1 otherwise)." $ property $
        \i' seed -> let i = min i' 10000
                        out = doRand (rollStep4 i) seed :: Int in case compare i 0 of
            GT -> out > 0
            _ -> out == 1
    it "rollChocolate x is always in range -x to x." $ property $
        \x' seed -> let x = min x' 10000
                        out = doRand (rollChocolate x) seed :: Int in case compare x 0 of
            GT -> out >= (-x) && out <= x
            _ -> out == 0
    it "rollOneHack is always positive with positive input (0 otherwise)." $ property $
        \x' seed -> let x = min x' 10000
                        out = doRand (rollOneHack x) seed :: Int in case compare x 0 of
            GT -> out > 0
            _ -> out == 0
    it "rollHack is always >=x for positive x and y, 0 otherwise." $ property $
        \x' y' seed -> let x = (min x' 10000)
                           y = (min y' 10000)
                           out = doRand (rollHack x y) seed :: Int in case (compare x 0, compare y 0) of
            (GT, GT) -> out >= x
            (_,_) -> out == 0
    it "rollExponent is always positive." $ property $
        \x lv seed -> doRand (rollExponent x lv) seed >= (1 :: Int)
    it "rollLuck is always in range with x > 1, and 1 otherwise" $ property $
        \x luck seed -> let out = doRand (rollLuck x luck) seed :: Int in if x > 1
            then out >= 0 && out <= (x-1)
            else out == 1
    it "rollZ is always non-negative." $ property $
        \i lv seed -> doRand (rollZ i lv) seed >= (0 :: Int)
