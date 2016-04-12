{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| This tests the Util.PPFOV module.
-}
module Util.PPFOVTests {-(
    ppfovTests
    )-} where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as S
import Control.Monad
import Control.RNG
import Control.Monad.Random
import Control.Monad.State

import Data.Location
import Util.PPFOVNext
import Data.PCGen

-- -- -- -- --
-- Instance for an Arbitrary Location.
-- -- -- -- --

-- | Orphan, but that's alright.
instance Arbitrary Location where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Location (x,y))

-- -- -- -- --
-- FOV Tests
-- -- -- -- --

diagVision :: Int -> Int -> VisionBlocked
diagVision x y = (\loc -> getLocX loc == x || getLocY loc == y)

diagRequired :: Int -> Int -> [Location]
diagRequired x y = [Location (x+1,y+1),Location (x+1,y-1),Location (x-1,y+1),Location (x-1,y-1)]

pillarVision :: Int -> Int -> VisionBlocked
pillarVision x y = \loc -> getLocX loc == x && getLocY loc == y

pillarPairs :: Int -> Int -> [(Location,Location)]
pillarPairs locx locy = do
    x <- [-1 .. 1]
    y <- [-1 .. 1]
    guard $ not (x == 0 && y == 0)
    return (Location (locx+x,locy+y),Location (locx-x,locy-y))

kuoClear :: Location -> Int -> [(VisionBlocked,Location)]
kuoClear start corLength = do
    dx <- [-1 .. 1]
    dy <- [-1 .. 1]
    isVert <- [True,False]
    let x = getLocX start
    let y = getLocY start
    let end = if isVert
        then Location (x + (dx * 2), y + (dy * 2) + (corLength + 1) * dy)
        else Location (x + (dx * 2) + (corLength + 1) * dx, y + (dy * 2))
    let clearSpaces = execState (do
            modify' (S.insert start)
            if isVert
                then modify' $ S.insert $ Location (x,y+dy)
                else modify' $ S.insert $ Location (x+dx,y)
            if isVert
                then modify' $ S.insert $ Location (x,y+(dy*2))
                else modify' $ S.insert $ Location (x+(dx*2),y)
            forM_ [0 .. (corLength-1)] (\len -> if isVert
                then modify' $ S.insert $ Location (x + dx, y + (dy * 2) + (len * dy))
                else modify' $ S.insert $ Location (x + (dx * 2) + (len * dx), y + dy))
            if isVert
                then modify' $ S.insert $ Location (x + (dx * 2), y + (dy * 2) + (corLength - 1) * dy)
                else modify' $ S.insert $ Location (x + (dx * 2) + (corLength - 1) * dx, y + (dy * 2))
            if isVert
                then modify' $ S.insert $ Location (x + (dx * 2), y + (dy * 2) + (corLength) * dy)
                else modify' $ S.insert $ Location (x + (dx * 2) + (corLength) * dx, y + (dy * 2))
            modify' (S.insert end)
            ) (S.empty)
    return ((\loc -> not $ loc `S.member` clearSpaces),end)

ppfovTests :: Spec
ppfovTests = describe "PPFOV" $ do
    it "Always contains the starting Location." $ property $
        \start -> start `S.member` (computeFOV (const True) 10 start)
    it "Can see through a diagonal gap." $ property $
        \x y -> let out = computeFOV (diagVision x y) 10 (Location (x,y))
                    in all (`S.member` out) (diagRequired x y)
    it "Open room has the correct Location count." $ do
        {- This test is a little confusing in the math, so here goes:
        At range n, vision is blocked *beyond* n, and the scan goes out to n+1
         N  Range OpenSquares SeenSquares
        ---------------------------------
         0  1     1           9
         1  2     9           25
         2  3     25          49
         3  4     49          81
        -}
        map (\n -> S.size $ computeFOV (\loc -> abs (getLocX loc) > n || abs (getLocY loc) > n) (n+1) (Location (0,0))) [0..19]
            `shouldBe` map (\n -> (2*(n+1)+1)*(2*(n+1)+1)) [0..19]
    it "Can never see the far side of a pillar." $ property $
        \locx locy -> let pairs = pillarPairs locx locy
                          vision = pillarVision locx locy
                    in all (\(start,inverse) -> not $ inverse `S.member` computeFOV vision 3 start) pairs
    it "Can always see down a Kuo Corridor." $ property $
        \seed' -> let seed = seed' :: Int in fst $ runRand (do
            locx <- getRandomR (minBound + (510::Int), maxBound - 510)
            locy <- getRandomR (minBound + (510::Int), maxBound - 510)
            corLength <- getRandomR (2,100)
            let start = Location (locx,locy)
            return $ all (\(vision,end) -> end `S.member` computeFOV vision (corLength+10) start) (kuoClear start corLength)) (mkPCGen seed)
