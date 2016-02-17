{-# LANGUAGE BangPatterns #-}

module Data.Location2 (
    Location2 (),
    getLoc2X,
    getLoc2Y,
    mkLocation2) where

-- | A Location2 is just a newtype over a pair of Int values.
newtype Location2 = Location2 (Int,Int) deriving (Eq, Ord, Show)

-- | Returns the X component of the Location2.
getLocX :: Location -> Int
getLocX (Location (x,_)) = x

-- | Returns the Y component of the Location2.
getLocY :: Location -> Int
getLocY (Location (_,y)) = y

-- | Given two Int values, constructs a new Location2. Strict
--   in both arguments.
mkLocation :: Int -> Int -> Location
mkLocation !x !y = Location (x,y)
