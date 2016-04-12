{-# LANGUAGE ConstraintKinds #-}

{-| Provides many operations to randomly generate values in various ways within a
'MonadRandom' context. If you're not familiar, it's kinda like having a
'MonadState' context, but the state is always an instance of 'RandomGen'.

The basic way to use this stuff on its own is something like this:

> LudoLib> import Control.Monad.Random
> LudoLib> fst $ runRand (rollxdy 3 6) (mkPCGen 100)
> 9

But if you make your game's core monad an instance of MonadRandom then you can
mix these functions in anywhere of course.
-}
module Control.RNG where

import System.Random (RandomGen, Random, random, randomR)
import Control.Monad (replicateM)
import Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)
import Data.Foldable (toList)
import Data.Bits (Bits, (.&.))

{-| Almost every option here is polymorphic for any MonadRandom m, and with any
sort of output type that's both 'Random' and 'Integral'. This is just an alias
to keep all the signatures a little shorter. As far as I know, all the built in
Integral types are already instances of Random. Many functions here will always
give non-negative results and so they will work with Word, but a few *can* have
negative results, so be careful with those you are using a Word type for
whatever reason.
-}
type RandIntegralMonad i m = (Random i, Integral i, MonadRandom m)

{-| @rollOne n@, rolls a single die with the given number of sides. If n is less
than 1 then the output will always be 0, otherwise the output will be between 1
and n, inclusive on both ends.
-}
rollOne :: (RandIntegralMonad i m) => i -> m i
rollOne n
    | n < 1 = return 0
    | otherwise = getRandomR (1,n)

{-| @rollxdy x y@, rolls x y-sided dice, via 'rollOne', returning the total.
-}
rollxdy :: (RandIntegralMonad i m) => i -> i -> m i
rollxdy count sides = sum <$> rollxdy' count sides

{-| @rollxdy' x y@, rolls x y-sided dice, via 'rollOne', returning the list of
individual results.
-}
rollxdy' :: (RandIntegralMonad i m) => i -> i -> m [i]
rollxdy' count sides = replicateM (fromIntegral count) (rollOne sides)

{-| @rollPool count sides tn@, rolls the given number of dice with the given
number of sides, retuning the number of dice that are equal to or above the tn
given. If you need to look at the pool results more closely than that (such as
for glitches or roll agains) then you'll just have to use 'rollxdy'' yourself.
-}
rollPool :: (RandIntegralMonad i m) => i -> i -> i -> m i
rollPool count sides tn = fromIntegral . length . filter (>= tn) <$> rollxdy' count sides

{-| @rollChance c@, rolls a d100 and returns if the value rolled is equal to or
less than the input given. @rollChance 56@ would have a 56% chance of being
True, for example.
-}
rollChance :: (RandIntegralMonad i m) => i -> m Bool
rollChance c = do
    r <- rollOne 100
    return (r <= c)

{-| @rollChanceIn times outOf@, rolls a die with a number of sides equal to the
second value, and returns if the result is equal to or less than the first
value. @rollChanceIn 3 8@ would have a 3/8ths chance of returning True, for
example.
-}
rollChanceIn :: (RandIntegralMonad i m) => i -> i -> m Bool
rollChanceIn times outOf = do
    r <- rollOne outOf
    return (r <= times)

{-| @pickRandom foldable@, picks a random element out of a Foldable of any finite
size. A non-empty foldable will always return a Just value, while an empty
foldable will always return a Nothing value.

This uses 'length' to figure out how many options there are in the foldable, and
so if you pass an infinite foldable you'll be stuck in an infinite loop.
-}
pickRandom :: (MonadRandom m, Foldable t) => t a -> m (Maybe a)
pickRandom foldable = do
    let len = length foldable
    r <- rollOne len
    let list = toList foldable
    if len > 0
        then return $ Just $ (list !! (r-1))
        else return Nothing

{-| @pickRandomNonempty foldable@, selects a random element from a finite,
non-empty Foldable. This is effectively the unsafe version of 'pickRandom'. If
the Foldable is empty, this uses 'fail' with a message (so you still might be
okay if you're in a failable monad?). If there's any doubt, use 'pickRandom'.

This uses 'length' to know how many elements are in the Foldable, so if you pass
an infinite foldable you're boned.
-}
pickRandomNonempty :: (MonadRandom m, Foldable t) => t a -> m a
pickRandomNonempty foldable = do
    let len = length foldable
    r <- rollOne len
    let list = toList foldable
    if len > 0
        then return (list !! (r-1))
        else fail "pickRandomNonempty cannot select from an empty foldable."

{-| @rollExplode n@, rolls an n sided die via 'rollOne', rolling again and adding
to the total if the maximum result is rolled. Rerolls can add into the total any
number of times. Doesn't reroll if the input is equal to or less than 1. This is
primarily useful for 'rollStep4', but you can use it directly if you want.
-}
rollExplode :: (RandIntegralMonad i m) => i -> m i
rollExplode n = do
    r <- rollOne n
    if r == n && n > 1
        then do
                b <- rollExplode n
                return (r+b)
        else return r

{-| @rollStep4 x@, rolls a step number according to the 4e step chart. If you
don't already know what that means then you probably don't care about this
function. The minimum result is 1.
-}
rollStep4 :: (RandIntegralMonad i m) => i -> m i
rollStep4 x = do
    rolls <- mapM rollExplode (sides x)
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

{-| @rollChocolate x@, rolls a number of dice where each die has a +1, 0, or
-1 on it, and returns the total. Obviously this is prone to giving negative
results, so it won't work properly with Word based types.
-}
rollChocolate :: (RandIntegralMonad i m) => i -> m i
rollChocolate x
    | x > 0 = (subtract (2 * x)) <$> rollxdy x 3
    | otherwise = return 0

{-| @rollOneHack sides@, rolls a number of dice, each with the given number of
sides.  If a die rolls maximum, another is rolled and added into the total with
a -1. The sides of the extra die depends on the original: 1-19 -> same as the
input; 20-99 -> 6; 100+ -> 20
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

{-| @rollHack numDice numSides@, rolls several hack dice at once, giving the
total.
-}
rollHack :: (RandIntegralMonad i m) => i -> i -> m i
rollHack numDice numSides =
    sum <$> mapM rollOneHack (replicate (fromIntegral numDice) numSides)

{-| @rollExponent x lv@, rolls a number that's 1 or more, with an exponentially
decreasing chance of being each value above 1. Starting at 1, there's a 1/x
chance of the total being bumped up by one. Once a check fails then that's the
total. Even in very lucky cases, there's a cap on the total of lv/3 (minimum cap
of 5). This is intended for things like magic modifiers on equipment in RPGs.
-}
rollExponent :: (RandIntegralMonad i m) => i -> i -> m i
rollExponent x lv
    | x <= 0 = return 1
    | otherwise = do
        let limit = if lv < 15 then 5 else lv `div` 3
        roll <- rollExplode x
        return $ min limit ((roll `div` x) + 1)

{-| @rollLuck x luck@, rolls a result between 0 and (x-1). Higher luck makes the
result more likely to be 0. Under this rolling method, a result of 0 is
considered a good thing.

Luck can be positive or negative, but it's beneficial effect is capped at +49
(there's no negative cap). x can be any value greater than 1. If x is 1 or less,
then the result is always 1.
-}
rollLuck :: (RandIntegralMonad i m, Bits i) => i -> i -> m i
rollLuck x luck
    | x > 1 = do
        i <- getRandomR (0,(x-1))
        bitMask <- getRandomR (0,(50-(min luck 49)))
        if (luck .&. bitMask) > 0
            then if x <= 15 && luck >= (-5)
                    then return $ min (max 0 (i-(luck `div` 3))) (x-1)
                    else return $ min (max 0 (i-luck)) (x-1)
            else return i
    | otherwise = return 1

{-| @rollZ i lv@, rolls a totally bizarre number. The lv value is used as part of
a call to 'rollExponent'. If i is less than 1, the result is always
1. Otherwise, the result is some non-negative value. Not much more can be said.
-}
rollZ :: (RandIntegralMonad i m) => i -> i -> m i
rollZ i lv
    | i < 1 = return 1
    | otherwise = do
        tmpAdd <- getRandomR (0,999)
        tmpMult <- rollExponent 4 lv
        let tmp = (1000 + tmpAdd) * tmpMult
        bool <- getRandom
        if bool
            then return $ (i * tmp) `div` 1000
            else return $ (i * 1000) `div` tmp
