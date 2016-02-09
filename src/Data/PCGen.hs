{-# LANGUAGE CPP #-}

{-
Based upon the C and C++ versions by M.E. O'Neill of pcg-random.org
Her versions are Copyright 2014 to her, used under the Apache 2.0 License
-}

{-|
This contains a random number generateor based upon the C and C++ versions 
of the concept written by M.E. O'Neill of <pcg-random.org>. Her versions
are Copyright 2014 to her. I'm not sure how cross-language copyright works,
if it applies at all, but her work is used under the Apache 2.0 License.

There are two actual versions provided:
* 'PCGen32' provides 32 bits of output per step by holding 128 bits of
internal data, half of which changes each step, and the other half of which
is fixed during the lifetime of the generator.
* 'PCGen64' is a version which provides 64 bits of output per step by holding
two different PCGen32 values, running them both one step, and then combining
both of the results into a single result.
* 'PCGen' is an architecture specific type alias that always matches the
output type of the generator to your current machine width.

Use 'mkPCGen', or one of the width-specific variants, to create new PCGen
values, and then treat it just like any other 'RandomGen' value.
-}
module Data.PCGen where

import System.Random
import Data.Bits
import Data.Word
import Data.Int

-- -- -- -- --
-- PCGen32 Section
-- -- -- -- --

{-| A Permuted Congruential Generator that produces 32-bits of
output per step. The generator uses 64-bits of state that changes
from use to use, as well as 64-bits as an incrementation value
that *does not* change from use to use. The period is 2^64, and
2^63 distinct increment values each produce a unique sequence.
The Inc value must ALWAYS be odd, so use mkPCGen32 to ensure that.
-}
data PCGen32 = PCGen32 {
    getState32 :: Word64,
    getInc32 :: Word64
    } deriving (Eq, Ord, Show)

-- | The Inc value on a PCGen32 must always be odd, so use this
--   to make sure that's the case. Runs the generator once to advance
--   the seed to a more useful value (you usually get 0 otherwise).
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

-- | Merges two Word32s into a single Word64. The first value is the
--   higher order bits and the second value is the lower order bits.
mix32 :: Word32 -> Word32 -> Word64
mix32 a b = ((fromIntegral a) `shiftL` 32) .|. (fromIntegral b)

-- | Mostly what you'd expect. The output Word32 is turned into an Int32 before
--   it's turned into an Int, so that the output is both positive and negative,
--   which matches up better with how the PCGen64 version works.
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
            stateA = (getState32 nGen4) `rotateR` 5
            stateB = (getState32 nGen4) `rotateR` 3
            incA = mix32 q w
            incB = mix32 e r
            outA = mkPCGen32 stateA incA
            outB = mkPCGen32 stateB incB

-- | Randomly generates PCGen32 values. For randomR, the range denotes the
--   range of the new gen's inc value.
instance Random PCGen32 where
    random gen = (mkPCGen32 stateWord incWord,gen'')
        where (stateWord,gen') = random gen
              (incWord,gen'') = random gen
    randomR (lowGen,highGen) gen = (mkPCGen32 stateWord incWord,gen'')
        where (stateWord,gen') = random gen
              (incWord,gen'') = randomR (getInc32 lowGen,getInc32 highGen) gen

-- -- -- -- --
-- PCGen64 Section
-- -- -- -- --

{-| A Permuted Congruential Generator that produces 64-bits of
output per step. The generator uses two PCGen32 that are run along
side each other during each step.
The Inc values must ALWAYS be odd, so use mkPCGen64 to ensure that.
-}
data PCGen64 = PCGen64 {
    getGenA :: PCGen32,
    getGenB :: PCGen32
    } deriving (Eq, Ord, Show)

-- | The Inc value on a PCGen64 must always be odd, so use this
--   to make sure that's the case. The generators should also have
--   different increment values from each other, which this also checks.
--   If they are the same, the second generator's inc value is bumped
--   up by two, so that the generators are out of phase with each other.
mkPCGen64 :: Word64 -> Word64 -> Word64 -> Word64 -> PCGen64
mkPCGen64 sa ia sb ib = out
    where out = snd $ stepPCGen64 $ PCGen64 (mkPCGen32 sa (ia+x)) (mkPCGen32 sb ib)
          x = if ia == ib then 2 else 0

-- | Given a PCGen64, produces the next random 64 bits, and the next PCGen64.
stepPCGen64 :: PCGen64 -> (Word64, PCGen64)
stepPCGen64 (PCGen64 genA genB) = (out, outGen)
    where (resultA,newGenA) = stepPCGen32 genA
          (resultB,newGenB) = stepPCGen32 genB
          outGen = PCGen64 newGenA newGenB
          out = mix32 resultA resultB

-- | Pretty much what you'd expect. The results of genRange get truncated on
--   a 32-bit machine, because Int becomes Int32
instance RandomGen PCGen64 where
    next gen = (outInt, nextGen)
        where (outWord64, nextGen) = stepPCGen64 gen
              outInt = fromIntegral outWord64
    genRange _ = (fromIntegral (minBound :: Int),fromIntegral (maxBound :: Int))
    split (PCGen64 genA genB) = (left, right)
        where -- no statistical foundation for this!
            left = PCGen64 a1 b1
            right = PCGen64 a2 b2
            (a1,a2) = split genA
            (b1,b2) = split genB

-- | Randomly generates PCGen64values. For randomR, the genA of the range denotes
--   the range of the new gen's inc value.
instance Random PCGen64 where
    random gen = (mkPCGen64 stateWord incWord stateWord incWord,gen'')
        where (stateWord,gen') = random gen
              (incWord,gen'') = random gen
    randomR (lowGen,highGen) gen = (mkPCGen64 stateWord incWord stateWord incWord,gen'')
        where (stateWord,gen') = random gen
              (incWord,gen'') = randomR ((getInc32.getGenA) lowGen,(getInc32.getGenA) highGen) gen

-- -- -- -- --
-- Section to make a "PCGen" type that always matches the local bit-width
-- -- -- -- --

#ifdef SixtyFourBit
type PCGen = PCGen64

mkPCGen :: (Integral i) => i -> PCGen
mkPCGen i = let a = fromIntegral i in mkPCGen64 a a a a
#else
type PCGen = PCGen32

mkPCGen :: (Integral i) => i -> PCGen
mkPCGen i = let a = fromIntegral i in mkPCGen32 a a
#endif
