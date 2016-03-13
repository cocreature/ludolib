{-# LANGUAGE CPP, MagicHash, BangPatterns #-}

{-| This contains a random number generateor based upon the C and C++ versions of
the concept written by M.E. O'Neill of <pcg-random.org>. Her versions are
Copyright 2014 to her. I'm not sure how cross-language copyright works, if it
applies at all, but her work is used under the Apache 2.0 License.

There are several versions provided:

* 'PCGen32' provides 32 bits of output by holding on to two Word64 values
internally. One value is the "state" of the generator, which is different for
each step of the generator as you call 'next' successive times. The other value
is an "increment" value, which stays the same across the different generations
of a particular generator. Each possible increment value produces a different
ordering of outputs, but an increment value must be odd, so overall there's 2^63
possible output streams. Each output stream has 2^64 different outputs before it
loops.

* 'PCGen64' is a version which provides 64 bits of output per step by holding
two different PCGen32 values, running them both one step for each step that it
has to make, and then combining the two Word32 values it gets into a single
Word64 value. The increment values on the two generators used aren't allowed to
be the same (just because it'd give sorta weird results, not for any concrete
math reason), but because there's two generators that each have 2^63 possible
increment values, a PCGen64 has (2^63)^2-(2^63) possible increment
configurations overall (~8.5*10^37). Each setup still only has 2^64 numbers per
stream though.

* 'PCGen' is an architecture-specific type alias that always matches the output
type of the generator to your current machine width, similar to how the 'Int'
type always matches size with your current machine width. It's possible to use a
PCGen64 on a 32-bit device, but your output gets truncated simply by your local
Int size, so it's a waste of your time.

In terms of speed, the generators are pretty zippy when you're on a 64-bit
machine. On my own computer, generating a list of 10 million values via
(evalState (replicateM 10000000 (state System.Random.next))) took 647ms for the
'PCGen32', 755ms for the 'PCGen64', and 1415ms when using the 'StdGen' type from
'System.Random'. Exact numbers vary from run to run of course, but you get the
general ballpark.

However, on a 32-bit machine the opposite is true: Because the internal datatype
is Word64, and that has to be emulated on a 32-bit machine, the operations end
up being significantly slower. Using an older P4 to test it was 5351ms for
'PCGen32', 9943ms for 'PCGen64', and only 3909ms for 'StdGen'.

If you want to always use the fastest generator available, you should use a CPP
macro where you first initialize the generator for your program and pick either
PCGen32 or StdGen, depending on your system.
-}
module Data.PCGen (
    -- * 32 bits of output
    PCGen32(),
    mkPCGen32,
    
    -- * 64 bits of output
    PCGen64(),
    mkPCGen64,
    
    -- * Locally sized output
    PCGen,
    mkPCGen
    ) where

import System.Random
import Data.Bits
import Data.Word
import Data.Int

import GHC.Exts
import GHC.Prim

#ifdef SixtyFourBit
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--    __ _  _     ____  _ _      _____           _                     
--   / /| || |   |  _ \(_) |    / ____|         | |                    
--  / /_| || |_  | |_) |_| |_  | (___  _   _ ___| |_ ___ _ __ ___  ___ 
-- | '_ \__   _| |  _ <| | __|  \___ \| | | / __| __/ _ \ '_ ` _ \/ __|
-- | (_) | | |   | |_) | | |_   ____) | |_| \__ \ ||  __/ | | | | \__ \
--  \___/  |_|   |____/|_|\__| |_____/ \__, |___/\__\___|_| |_| |_|___/
--                                      __/ |                          
--                                     |___/                           
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
{-
On 64-bit machines a Word# will have the same width as the Word64 that we want
to use, so we use Word# and primitive operations for a little bit of a speed
boost.
-}

-- -- -- -- --
-- PCGen32 Section (64-bit)
-- -- -- -- --

{-| A permuted congruential generator that produces 32 bits of randomness per step.
-}
data PCGen32 = PCGen32 Word# Word#

{-| Given two Word64 values, constructs a PCGen32 and runs the generator once,
giving you back the resulting generator. The generator is run once automatically
because most initial state values that a human picks end up giving 0 as the
first result, which is pretty un-random overall.
-}
mkPCGen32 :: Word64 -> Word64 -> PCGen32
mkPCGen32 state64 inc64 = let
    !(W# st) = fromIntegral state64
    !(W# inc) = fromIntegral inc64
    in snd $ next $ PCGen32 st (or# inc 1##)

{-| The genRange of a PCGen32on a 64 bit system is 0 through 4,294,967,295 (aka
0xFFFFFFFF, all lower 32 bits active).
-}
instance RandomGen PCGen32 where
    next !(PCGen32 st inc) = (I# (word2Int# (narrow32Word# w)),newGen) where
        xorShifted = uncheckedShiftRL# (xor# (uncheckedShiftRL# st 18#) st) 27#
        rot = uncheckedShiftRL# st 59#
        w = or# (uncheckedShiftRL# xorShifted (word2Int# rot)) (uncheckedShiftL# xorShifted (word2Int# (and# (minusWord# 0## rot) 31##)))
        newState = plusWord# (timesWord# st 6364136223846793005##) inc
        newGen = PCGen32 newState inc
    genRange _ = (I# 0#,I# 0xFFFFFFFF#)
    split !gen@(PCGen32 st inc) = (outA, outB)
        where -- no statistical foundation for this!
            !((I# q),nGen1) = next gen
            !((I# w),nGen2) = next nGen1
            !((I# e),nGen3) = next nGen2
            !((I# r),(PCGen32 stFour incFour)) = next nGen3
            !(W# stateA) = (W# stFour) `rotateR` 5
            !(W# stateB) = (W# stFour) `rotateR` 3
            incA = or# (uncheckedShiftL# (int2Word# q) 32#) (int2Word# w)
            incB = or# (uncheckedShiftL# (int2Word# e) 32#) (int2Word# r)
            outA = PCGen32 stateA (or# incA 1##)
            outB = PCGen32 stateB (or# incB 1##)

{-| randomR is the same as random for this type.
-}
instance Random PCGen32 where
    random gen = let
        (x,newGen) = random gen
        in (mkPCGen32 x x,newGen)
    randomR (_,_) gen = let
        (x,newGen) = random gen
        in (mkPCGen32 x x,newGen)

{-| PCGen32 values are equal when they have equal state incriment values.
-}
instance Eq PCGen32 where
    (==) !(PCGen32 stA incA) !(PCGen32 stB incB) = isTrue# (andI# (eqWord# stA stB) (eqWord# incA incB))

{-| PCGen32 are ordered by incriment, then by state. The order isn't intended to
be used for anything other than to potentially put PCGen32 values into a 'Set'.
-}
instance Ord PCGen32 where
    compare !(PCGen32 stA incA) !(PCGen32 stB incB) = case compare (W# incA) (W# incB) of
        LT -> LT
        EQ -> compare (W# stA) (W# stB)
        GT -> GT

{-| The show for a PCGen32 gives the GHCI expression to remake the same PCGen32
value.
-}
instance Show PCGen32 where
    show !(PCGen32 st inc) = "mkPCGen32 " ++ show (W# st) ++ " " ++ show (W# inc)

-- -- -- -- --
-- PCGen64 Section (64-bit)
-- -- -- -- --

{-| A Permuted Congruential Generator that produces 64-bits of output per step
-}
data PCGen64 = PCGen64 Word# Word# Word# Word#

{-| Assembles a new PCGen64 from the given data. Runs the generator once to put
the state values into a good spot, otherwise the first result after this is
called is usually 0 (for state values that humans pick).
-}
mkPCGen64 :: Word64 -> Word64 -> Word64 -> Word64 -> PCGen64
mkPCGen64 ain bin cin din = snd $ next $ PCGen64 a (plusWord# b x) c d
    where
        x = if isTrue# (eqWord# b d) then 2## else 0##
        !(PCGen32 a b) = mkPCGen32 ain bin
        !(PCGen32 c d) = mkPCGen32 cin din

{-| A PCGen64 has an output range across the entire range of 'Int'.
-}
instance RandomGen PCGen64 where
    next !(PCGen64 stA incA stB incB) = (I# (word2Int# w), newGen) where
        xorShiftedA = uncheckedShiftRL# (xor# (uncheckedShiftRL# stA 18#) stA) 27#
        xorShiftedB = uncheckedShiftRL# (xor# (uncheckedShiftRL# stB 18#) stB) 27#
        rotA = uncheckedShiftRL# stA 59#
        rotB = uncheckedShiftRL# stB 59#
        wSubA = narrow32Word# (or# (uncheckedShiftRL# xorShiftedA (word2Int# rotA)) (uncheckedShiftL# xorShiftedA (word2Int# (and# (minusWord# 0## rotA) 31##))))
        wSubB = narrow32Word# (or# (uncheckedShiftRL# xorShiftedB (word2Int# rotB)) (uncheckedShiftL# xorShiftedB (word2Int# (and# (minusWord# 0## rotB) 31##))))
        w = or# (uncheckedShiftL# wSubA 32#) wSubB
        newStateA = plusWord# (timesWord# stA 6364136223846793005##) incA
        newStateB = plusWord# (timesWord# stB 6364136223846793005##) incB
        newGen = PCGen64 newStateA incA newStateB incB
    genRange _ = (minBound,maxBound)
    split !(PCGen64 stA incA stB incB) = (left, right)
        where -- no statistical foundation for this!
            left = PCGen64 stateAL incAL stateBL incBL
            right = PCGen64 stateAR incAR stateBR incBR
            !((PCGen32 stateAL incAL),(PCGen32 stateAR incAR)) = split (PCGen32 stA incA)
            !((PCGen32 stateBL incBL),(PCGen32 stateBR incBR)) = split (PCGen32 stB incB)

{-| randomR is the same as random for this type.
-}
instance Random PCGen64 where
    random gen = let
        (x,newGen) = random gen
        in (mkPCGen64 x x x x,newGen)
    randomR (_,_) gen = let
        (x,newGen) = random gen
        in (mkPCGen64 x x x x,newGen)

{-| PCGen32 values are equal when they have equal state incriment values.
-}
instance Eq PCGen64 where
    (==) !(PCGen64 stA incA stB incB) !(PCGen64 stA' incA' stB' incB') =
        isTrue# (andI# (andI# (eqWord# stA stA') (eqWord# incA incA'))
                       (andI# (eqWord# stB stB') (eqWord# incB incB')))

{-| PCGen32 are ordered by incriment, then by state. The order isn't intended to
be used for anything other than to potentially put PCGen32 values into a 'Set'.
-}
instance Ord PCGen64 where
    compare !(PCGen64 stA incA stB incB) !(PCGen64 stA' incA' stB' incB') = case compare (W# incA) (W# incA') of
        LT -> LT
        GT -> GT
        EQ -> case compare (W# incB) (W# incB') of
                LT -> LT
                GT -> GT
                EQ -> case compare (W# stA) (W# stA') of
                        LT -> LT
                        GT -> GT
                        EQ -> compare (W# stB) (W# stB')

{-| The show for a PCGen32 gives the GHCI expression to remake the same PCGen32
value.
-}
instance Show PCGen64 where
    show !(PCGen64 stA incA stB incB) = "mkPCGen64 " ++
        show (W# stA) ++ " " ++ show (W# incA) ++ " " ++ show (W# stB) ++ " " ++ show (W# incB)

-- -- -- -- --
-- PCGen Section (64-bit)
-- -- -- -- --

{-| PCGen is a type alias for either PCGen32 or PCGen64 so that the output will
always use the range of the local machine register size.

This haddock was generated on a 64-bit machine, so PCGen is the same as 'PCGen64'
-}
type PCGen = PCGen64

{-| Constructs a new PCGen from any Integral value by using the value given as all
four values to 'mkPCGen64'.
-}
mkPCGen :: (Integral i) => i -> PCGen
mkPCGen i = let a = fromIntegral i in mkPCGen64 a a a a

#else
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--  ____ ___    ____  _ _      _____           _                     
-- |___ \__ \  |  _ \(_) |    / ____|         | |                    
--   __) | ) | | |_) |_| |_  | (___  _   _ ___| |_ ___ _ __ ___  ___ 
--  |__ < / /  |  _ <| | __|  \___ \| | | / __| __/ _ \ '_ ` _ \/ __|
--  ___) / /_  | |_) | | |_   ____) | |_| \__ \ ||  __/ | | | | \__ \
-- |____/____| |____/|_|\__| |_____/ \__, |___/\__\___|_| |_| |_|___/
--                                    __/ |                          
--                                   |___/                           
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
{-
On 32-bit machines, I really don't want to deal with the troubles of making two
Word# values work as a single Word64, so we'll just let our data stay wrapped
up. Things are still pretty fast, just not as fast.
-}

-- -- -- -- --
-- PCGen32 Section (32-bit)
-- -- -- -- --

{-| A Permuted Congruential Generator that produces 32-bits of output per step.
-}
data PCGen32 = PCGen32 {
    _state32 :: {-# UNPACK #-} !Word64, -- ^ The internal state of the generator.
    _inc32 :: {-# UNPACK #-} !Word64 -- ^ controls what number stream the generator moves along.
    } deriving (Eq, Ord, Show)

{-| The Inc value on a PCGen32 must always be odd, so this ensures that that is
always the case. It also runs the generator once to advance the seed to a more
useful value, otherwise you almost always get 0 as your first result.
-}
mkPCGen32 :: Word64 -> Word64 -> PCGen32
mkPCGen32 state inc = snd $ stepPCGen32 $ PCGen32 state (inc .|. 1)

-- | Given a PCGen32, produces the next random 32 bits, and the next PCGen32.
stepPCGen32 :: PCGen32 -> (Word32, PCGen32)
stepPCGen32 (PCGen32 state inc) = (w, newGen)
    where xorshifted = ((state `shiftR` 18) `xor` state) `shiftR` 27
          rot = fromIntegral $ state `shiftR` 59
          w = fromIntegral $ (xorshifted `shiftR` rot) .|. (xorshifted `shiftL` ((-rot) .&. 31))
          newState = state * 6364136223846793005 + inc
          newGen = PCGen32 newState inc

{-| Mostly what you'd expect. The output Word32 is turned into an Int32 before
it's turned into an Int, so that the output is both positive and negative, which
matches up better with how the PCGen64 version works.
-}
instance RandomGen PCGen32 where
    next gen = (outInt, nextGen)
        where (outWord32, nextGen) = stepPCGen32 gen
              outInt = fromIntegral (fromIntegral outWord32 :: Int32)
    genRange _ = (fromIntegral (minBound :: Int32),fromIntegral (maxBound :: Int32))
    split gen@(PCGen32 state inc) = (outA, outB)
        where -- no statistical foundation for this!
            (q,nGen1) = stepPCGen32 gen
            (w,nGen2) = stepPCGen32 nGen1
            (e,nGen3) = stepPCGen32 nGen2
            (r,nGen4) = stepPCGen32 nGen3
            stateA = (_state32 nGen4) `rotateR` 5
            stateB = (_state32 nGen4) `rotateR` 3
            incA = ((fromIntegral q) `shiftL` 32) .|. (fromIntegral w)
            incB = ((fromIntegral e) `shiftL` 32) .|. (fromIntegral r)
            outA = mkPCGen32 stateA incA
            outB = mkPCGen32 stateB incB

{-| randomR is the same as random for this type.
-}
instance Random PCGen32 where
    random gen = let
        (x,newGen) = random gen
        in (mkPCGen32 x x,newGen)
    randomR (_,_) gen = let
        (x,newGen) = random gen
        in (mkPCGen32 x x,newGen)

-- -- -- -- --
-- PCGen64 Section (32-bit)
-- -- -- -- --

{-| A Permuted Congruential Generator that produces 64-bits of output per step
-}
data PCGen64 = PCGen64 {
    _genA :: {-# UNPACK #-} !PCGen32, -- ^ One of the two internal generators the PCGen64 uses.
    _genB :: {-# UNPACK #-} !PCGen32 -- ^ The other of the two internal generators the PCGen64 uses.
    } deriving (Eq, Ord, Show)

{-| The Inc value on a PCGen64 must always be odd, so this ensures that that is
the case. The generators should also have different increment values from each
other, which this also checks.  If the values given woule have them be the same,
the second generator's inc value is bumped up, so that the generators are always
out of phase with each other.
-}
mkPCGen64 :: Word64 -> Word64 -> Word64 -> Word64 -> PCGen64
mkPCGen64 sa ia sb ib = out
    where out = snd $ stepPCGen64 $ PCGen64 (mkPCGen32 sa (ia+x)) (mkPCGen32 sb ib)
          x = if (ia .|. 1) == (ib .|. 1) then 2 else 0

-- | Given a PCGen64, produces the next random 64 bits, and the next PCGen64.
stepPCGen64 :: PCGen64 -> (Word64, PCGen64)
stepPCGen64 (PCGen64 genA genB) = (out, outGen)
    where (resultA,newGenA) = stepPCGen32 genA
          (resultB,newGenB) = stepPCGen32 genB
          outGen = PCGen64 newGenA newGenB
          out = ((fromIntegral resultA) `shiftL` 32) .|. (fromIntegral resultB)

{-| Pretty much what you'd expect. The results of genRange get truncated on a
32-bit machine, because Int becomes Int32
-}
instance RandomGen PCGen64 where
    next gen = (outInt, nextGen)
        where (outWord64, nextGen) = stepPCGen64 gen
              outInt = fromIntegral outWord64
    genRange _ = (fromIntegral (minBound :: Int),fromIntegral (maxBound :: Int))
    split (PCGen64 genA genB) = (left, right)
        where -- no statistical foundation for this!
            left = mkPCGen64 (_state32 a1) (_inc32 a1) (_state32 b1) (_inc32 b1)
            right = mkPCGen64 (_state32 a2) (_inc32 a2) (_state32 b2) (_inc32 b2)
            (a1,a2) = split genA
            (b1,b2) = split genB

{-| randomR is the same as random for this type.
-}
instance Random PCGen64 where
    random gen = let
        (x,newGen) = random gen
        in (mkPCGen64 x x x x,newGen)
    randomR (_,_) gen = let
        (x,newGen) = random gen
        in (mkPCGen64 x x x x,newGen)

-- -- -- -- --
-- PCGen Section (32-bit)
-- -- -- -- --

{-| PCGen is a type alias for either PCGen32 or PCGen64 so that the output will
always use the range of the local machine register size.

This haddock was generated on a 32-bit machine, so PCGen is the same as 'PCGen64'
-}
type PCGen = PCGen32

{-| Constructs a new PCGen from any Integral value by using the value given as
both values to 'mkPCGen32'.
-}
mkPCGen :: (Integral i) => i -> PCGen
mkPCGen i = let a = fromIntegral i in mkPCGen32 a a

#endif
