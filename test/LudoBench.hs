{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Criterion.Main

import Control.DeepSeq

import Data.Location
import qualified Util.PPFOV as Old
import qualified Util.PPFOVNew as New

-- | Orphan, but that's alright.
instance NFData Location where
    -- rnf :: a -> ()
    rnf l@(Location (x,y)) = x `seq` y `seq` l `seq` ()

vb = (\(Location (x,y)) -> if x > 30 || y > 30 then True else False)

main = defaultMain [
    bench "Old" $ nf (Old.computeFOV vb 50) (Location (0,0)),
    bench "New" $ nf (New.computeFOV vb 50) (Location (0,0))
    ]
