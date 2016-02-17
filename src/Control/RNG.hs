{-# LANGUAGE ConstraintKinds #-}

module Control.RNG where

import System.Random (RandomGen, Random, random, randomR)
import Control.Monad (replicateM)
import Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)
import Data.Foldable (toList)
import Data.Bits (Bits, (.&.))

type RandIntegralMonad i m = (Random i, Integral i, MonadRandom m)

{-| Rolls a single die with the given number of sides. If the input is less
than one then the output will always be zero. Otherwise the output will be
between one and the input, inclusive on both ends.
-}
rollOne :: (RandIntegralMonad i m) => i -> m i
rollOne n
    | n < 1 = return 0
    | otherwise = getRandomR (1,n)

{-| Given (count,sides), rolls the number of dice specified, each
with the number of sides specified. Returns the results as a list of
individual rolls.
-}
rollxdy' :: (RandIntegralMonad i m) => (i,i) -> m [i]
rollxdy' (count,sides) = replicateM (fromIntegral count) (rollOne sides)

{-| Given (count,sides), rolls the number of dice specified, each
with the number of sides specified. Returns the sum.
-}
rollxdy :: (RandIntegralMonad i m) => (i,i) -> m i
rollxdy (count,sides) = sum <$> rollxdy' (count,sides)

{-| Given (count,sides,tn), rolls the number of dice with the given sides, and
then returns the total of how many dice were equal to or greater than the target
number specified.
-}
rollPool :: (RandIntegralMonad i m) => (i,i,i) -> m i
rollPool (count,sides,tn) = fromIntegral . length . filter (>= tn) <$> rollxdy' (count,sides)

{-| Rolls a d100 and returns True if the value rolled is equal to
or less than the input given.
-}
rollChance :: (RandIntegralMonad i m) => i -> m Bool
rollChance c = do
    r <- rollOne 100
    if r <= c
        then return True
        else return False

{-| Rolls a die with a number of sides equal to the second value, and returns
True if the result is equal to or less than the first value. In other words
"rollChanceIn 3 8" would have a 3 in 8 chance of returning True.
-}
rollChanceIn :: (RandIntegralMonad i m) => i -> i -> m Bool
rollChanceIn times outOf = do
    r <- rollOne outOf
    if r <= times
        then return True
        else return False

{-| Selects a random element from a non-empty Foldable. If the Foldable
is empty, an error will occur.
-}
pickRandom :: (MonadRandom m, Foldable t) => t a -> m a
pickRandom foldable = do
    let len = length foldable
    r <- rollOne len
    let list = toList foldable
    if len > 0
        then return (list !! (r-1))
        else fail "pickRandom cannot select from an empty foldable."

{-| Like pickRandom, but with type safety! If your target Foldable might be
empty you should use this.
-}
pickRandomSafe :: (MonadRandom m, Foldable t) => t a -> m (Maybe a)
pickRandomSafe foldable = do
    let len = length foldable
    r <- rollOne len
    let list = toList foldable
    if len > 0
        then return $ Just $ (list !! (r-1))
        else return Nothing

{-| Rolls an exploding die. That is, if the die gets a maximum result then
another is rolled and added into the total. As a catch, won't explode if
N is 1, otherwise that'd just be an infinite loop.
-}
rollxpl :: (RandIntegralMonad i m) => i -> m i
rollxpl n = do
    r <- rollOne n
    if r == n && n > 1
        then do
                b <- rollxpl n
                return (r+b)
        else return r

{-| Rolls a step number according to the 4e step chart.
-}
rollStep4 :: (RandIntegralMonad i m) => i -> m i
rollStep4 x = do
    rolls <- mapM rollxpl (sides x)
    case x of
        1 -> return (max (sum $ (-2) : rolls) 1)
        2 -> return (max (sum $ (-1) : rolls) 1)
        _ -> return (sum rolls)
    where sides x' = case x' of
            1 -> [4]
            2 -> [4]
            3 -> [4]
            4 -> [6]
            5 -> [8]
            6 -> [10]
            7 -> [12]
            8 -> [6,6]
            9 -> [8,6]
            10 -> [8,8]
            11 -> [10,8]
            12 -> [10,10]
            13 -> [12,10]
            14 -> [12,12]
            15 -> [12,6,6]
            16 -> [12,8,6]
            17 -> [12,8,8]
            18 -> [12,10,8]
            z | z > 0     -> 20 : sides (z-11)
              | otherwise -> [1]

{-| Rolls a number of dice where each die has a +1, 0, or -1
on it, and returns the total. Obviously this is prone to giving negative
results, so it won't work with Word and stuff.
-}
rollChocolate :: (RandIntegralMonad i m) => i -> m i
rollChocolate numDice
    | numDice > 0 = (subtract (2 * numDice)) <$> rollxdy (numDice,3)
    | otherwise = return 0

{-| Rolls a number of dice, each with the given number of sides.
If a die rolls maximum, another is rolled and added into the total
with a -1. The sides of the extra die depends on the original:
1-19 -> same as the input; 20-99 -> 6; 100+ -> 20
-}
rollOneHack :: (RandIntegralMonad i m) => i -> m i
rollOneHack sides = do
    r <- rollOne sides
    if r == sides && sides > 1
        then do
            b <- rollOneHack p
            return (r + b - 1)
        else return r
    where
        p = case sides of
            n | n > 99 -> 20
              | n > 19 -> 6
              | otherwise -> n

{-| Rolls several hack dice at once.
-}
rollHack :: (RandIntegralMonad i m) => (i,i) -> m i
rollHack (numDice,numSides) =
    sum <$> mapM rollOneHack (replicate (fromIntegral numDice) numSides)

{-| Rolls a number that's 1 or more, with an exponentially
decreasing chance of being each number above 1 (controlled by x),
and a cap of lv/3 (minimum cap of 5).
-}
rollExp :: (RandIntegralMonad i m) => i -> i -> m i
rollExp x lv
    | x <= 0 = return 1
    | otherwise = do
        let limit = if lv < 15 then 5 else lv `div` 3
        roll <- rollxpl x
        return $ min limit ((roll `div` x) + 1)

{-| Rolls a number between 0 and (x-1), influenced by luck. Higher luck makes
the result more likely to be 0. Luck should not exceed 50, and x should not
be less than 1.
-}
rollLuck :: (RandIntegralMonad i m, Bits i) => i -> i -> m i
rollLuck x luck = do
    i <- getRandomR (0,(x-1))
    bitMask <- getRandomR (0,(50-luck))
    if (luck .&. bitMask) > 0
        then if x <= 15 && luck >= (-5)
                then return $ min (max 0 (i-(luck`div`3))) (x-1)
                else return $ min (max 0 (i-luck)) (x-1)
        else return i

{-| Rolls a totally bizarre number. It might be around half the
input, or it might not be. Not much more can be said.
-}
rollZ :: (RandIntegralMonad i m) => i -> i -> m i
rollZ i lv
    | i < 1 = return 1
    | otherwise = do
        tmpAdd <- getRandomR (0,999)
        tmpMult <- rollExp 4 lv
        let tmp = (1000 + tmpAdd) * tmpMult
        bool <- getRandom
        if bool
            then return $ (i * tmp) `div` 1000
            else return $ (i * 1000) `div` tmp
