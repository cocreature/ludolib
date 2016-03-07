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

main = defaultMain [
    bench "Old" $ nf (Old.computeFOV (const True) 5) (Location (0,0)),
    bench "New" $ nf (New.computeFOV (const True) 5) (Location (0,0))
    ]
