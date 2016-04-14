
{-| Makes dungeons via a cellular automata style technique.

INCOMPLETE AT THE MOMENT
-}
module Util.AutomataGen where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as VM

import Control.Monad.ST
import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad.Primitive
import Control.Monad.Trans.Class

import Data.Bool

import Data.Location

{-| There's a lot of features a dungeon can have in a game, but for cellular
automata work we just need to know what space we're working with and if there's
a wall or not in each cell.
-}
data SimpleDungeon = SimpleDungeon {
    _getWidth :: Int,
    _getHeight :: Int,
    _getWalls :: Vector Bool
    } deriving (Eq, Ord, Show)

{-| Groups a list into sub-lists that are each the specified length. If the
given length doesn't divide the input evenly then the final list will be
shorter.
-}
groupsOf :: Int -> [a] -> [[a]]
groupsOf x [] = []
groupsOf x elems = take x elems : groupsOf x (drop x elems)

{-| Turns a 'SimpleDungeon' into a String suitable for use with 'putStrLn'
-}
formatSimple :: SimpleDungeon -> String
formatSimple d = let
    width = _getWidth d
    height = _getHeight d
    toChar = bool '.' '#'
    chars = map toChar (V.toList $ _getWalls d)
    in unlines $ groupsOf width chars

-- | Makes a SimpleDungeon where all cells are the value given.
mkDungeon :: Int -> Int -> Bool -> SimpleDungeon
mkDungeon width height cell = SimpleDungeon width height (V.replicate (width*height) cell)

{-| Simplies the process of checking for a wall. Out of bounds locations default
to True, which simplifies FOV stuff.
-}
hasWall :: SimpleDungeon -> Location -> Bool
hasWall d loc = let
    x = getLocX loc
    y = getLocY loc
    width = _getWidth d
    height = _getHeight d
    cells = _getWalls d
    index = x + y*width
    in x < 0 || y < 0 || x >= width || y >= height || cells V.! index

{-| Makes a dungeon that's blank except for an outline of walls around the
edges.
-}
mkOutline :: Int -> Int -> SimpleDungeon
mkOutline width height = SimpleDungeon width height $ runST $ do
    d <- VM.replicate (width*height) False
    forM_ [(x,y) | x <- [0 .. width-1], y <- [0 .. height-1]] (\(x,y) ->
        when (x == 0 || y == 0 || x == width-1 || y == height-1) $
            VM.write d (x + y*width) True)
    V.unsafeFreeze d
