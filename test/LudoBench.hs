{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Criterion.Main

import Control.DeepSeq

import Data.Location
import qualified Util.PPFOV as ST

-- | Orphan, but that's alright.
instance NFData Location where
    -- rnf :: a -> ()
    rnf l@(Location (x,y)) = x `seq` y `seq` l `seq` ()

range = 50

openVision = (\(Location (x,y)) -> if x > 30 || y > 30 then True else False)

crowdedVision = (\(Location (x,y)) -> if (even x && y `mod` 4 == 0) || y < (negate 13) then True else False)

main = defaultMain [
    bench "PPFOV with ST, Open" $ nf (ST.computeFOV openVision range) (Location (0,0)),
    bench "PPFOV with ST, Crowded" $ nf (ST.computeFOV crowdedVision range) (Location (0,0))
    ]
