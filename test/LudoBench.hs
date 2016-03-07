{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Criterion.Main

import Control.DeepSeq

import Data.Location
import qualified Util.PPFOV as Classic
import qualified Util.PPFOVST as ST

-- | Orphan, but that's alright.
instance NFData Location where
    -- rnf :: a -> ()
    rnf l@(Location (x,y)) = x `seq` y `seq` l `seq` ()

range = 50

visionB = (\(Location (x,y)) -> if x > 30 || y > 30 then True else False)

main = defaultMain [
    bench "Classic" $ nf (Classic.computeFOV visionB range) (Location (0,0)),
    bench "With ST" $ nf (ST.computeFOV visionB range) (Location (0,0))
    ]
