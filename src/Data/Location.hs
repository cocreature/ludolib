{-# LANGUAGE BangPatterns #-}

{-| Many modules are concerned with the orientation of things, so we have a
Location data type.
-}
module Data.Location where

{-| A Location is a newtype over a pair of 'Int' values. Locations have an Ord
instance, but that's mostly so that they can go into Sets and doesn't actually
mean too much.
-}
newtype Location = Location (Int,Int) deriving (Eq, Ord, Show)

{-| Grabs out the X component of the Location.
-}
getLocX :: Location -> Int
getLocX (Location (x,_)) = x

{-| Grabs out the Y component of the Location.
-}
getLocY :: Location -> Int
getLocY (Location (_,y)) = y

{-| Makes a new Location, and is strict in both arguments.
-}
strictLocation :: Int -> Int -> Location
strictLocation !x !y = Location (x,y)

