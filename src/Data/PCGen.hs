{-# LANGUAGE CPP #-}

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
type always matches size with your current machine width.

Use 'mkPCGen', or one of the width-specific variants ('mkPCGen32' and
'mkPCGen64'), to create new generator values, and then just use the 'RandomGen'
methods.

In terms of speed, the generators are pretty zippy. On my machine, generating 1
million values took 90ms for the 'PCGen32', 110ms for the 'PCGen64', and 145ms when
using the 'StdGen' type from 'System.Random'.
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

-- -- -- -- --
-- PCGen32 Section
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
          w = fromIntegral $ xorshifted `rotateR` rot
          newState = state * 6364136223846793005 + inc
          newGen = PCGen32 newState inc

{-| Merges two Word32s into a single Word64. The first value is the higher order
bits and the second value is the lower order bits.
-}
mix32 :: Word32 -> Word32 -> Word64
mix32 a b = ((fromIntegral a) `shiftL` 32) .|. (fromIntegral b)

{-| Mostly what you'd expect. The output Word32 is turned into an Int32 before
it's turned into an Int, so that the output is both positive and negative, which
matches up better with how the PCGen64 version works.
-}
instance RandomGen PCGen32 where
    next gen = (outInt, nextGen)
        where (outWord32, nextGen) = stepPCGen32 gen
              outInt = fromIntegral (fromIntegral outWord32 :: Int32)
    genRange gen = (fromIntegral (minBound :: Int32),fromIntegral (maxBound :: Int32))
    split gen@(PCGen32 state inc) = (outA, outB)
        where -- no statistical foundation for this!
            (q,nGen1) = stepPCGen32 gen
            (w,nGen2) = stepPCGen32 nGen1
            (e,nGen3) = stepPCGen32 nGen2
            (r,nGen4) = stepPCGen32 nGen3
            stateA = (_state32 nGen4) `rotateR` 5
            stateB = (_state32 nGen4) `rotateR` 3
            incA = mix32 q w
            incB = mix32 e r
            outA = mkPCGen32 stateA incA
            outB = mkPCGen32 stateB incB

{-| Randomly generates PCGen32 values. For randomR, the range denotes the range of
the new gen's inc value.
-}
instance Random PCGen32 where
    random gen = let
        (x,newGen) = random gen
        in (mkPCGen32 x x,newGen)
    randomR (low,high) gen = let
        (x,newGen) = random gen
        (inc,_) = randomR (_inc32 low,_inc32 high) gen
        in (mkPCGen32 x inc,newGen)

-- -- -- -- --
-- PCGen64 Section
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
          out = mix32 resultA resultB

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

{-| Randomly generates PCGen64 values. For randomR, the genA of the range denotes
the range of the new gen's inc value, not that it really matters.
-}
instance Random PCGen64 where
    random gen = let
        (x,newGen) = random gen
        in (mkPCGen64 x x x x,newGen)
    randomR (low,high) gen = let
        (x,newGen) = random gen
        (inc,_) = randomR ((_inc32._genA) low,(_inc32._genA) high) gen
        in (mkPCGen64 x x x x,newGen)

-- -- -- -- --
-- Section to make a "PCGen" type that always matches the local bit-width
-- -- -- -- --

#ifdef SixtyFourBit

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
