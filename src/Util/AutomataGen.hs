
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

import Control.RNG

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

-- | Makes a SimpleDungeon where all cells are the value given.
mkDungeon :: Int -> Int -> Bool -> SimpleDungeon
mkDungeon width height cell = SimpleDungeon width height (V.replicate (width*height) cell)

{-| Makes a dungeon that's blank except for an outline of walls around the
edges.
-}
mkOutline :: Int -> Int -> SimpleDungeon
mkOutline width height = SimpleDungeon width height $ runST $ do
    d <- VM.replicate (width*height) False
    forM_ [(x,y) | x <- [0 .. width-1], y <- [0 .. height-1]] $ \(x,y) ->
        when (x == 0 || y == 0 || x == width-1 || y == height-1) $
            VM.write d (x + y*width) True
    V.unsafeFreeze d

{-| @mkRandom width height chance@

Makes a @width*height@ length Vector Bool where each cell has a @chance@
percent chance of being True. This is intended to make the initial maps to run
cellular automata computations with.
-}
mkRandom :: MonadRandom m => Int -> Int -> Int -> m (Vector Bool)
mkRandom width height chance = V.replicateM (width*height) (rollChance chance)

mkCaves :: MonadRandom m => Int -> Int -> m SimpleDungeon
mkCaves width height = do
    base <- mkRandom width height 40
    return $ SimpleDungeon width height $ runST $ do
        foo <- V.thaw base
        bar <- VM.replicate (width*height) False
        caveCopy width height foo bar
        caveCopy width height bar foo
        caveCopy width height foo bar
        caveCopy width height bar foo
        caveCopy width height foo bar
        --caveCopy width height bar foo
        --caveCopy' width height foo bar
        V.unsafeFreeze bar

{-| @ inRange (width,height) (cx,cy) range @

Gives the list of all indexes that are within the given range of the given
location, for a 1d space that's reprisenting the given width and height. Range
0 is the location itself, range 1 is all the places within 1 step, etc.
-}
inRange :: (Int,Int) -> (Int,Int) -> Int -> [Int]
inRange (width,height) (cx,cy) range = do
    x <- [cx-range .. cx+range]
    y <- [cy-range .. cy+range]
    guard $ not $ x < 0 || y < 0 || x >= width || y >= height
    return (x + y*width)

caveCopy :: PrimMonad m => Int -> Int -> VM.MVector (PrimState m) Bool -> VM.MVector (PrimState m) Bool -> m ()
caveCopy width height src dst = do
    forM_ [(x,y) | x <- [0 .. width-1], y <- [0 .. height-1]] $ \(x,y) -> do
        let oneIndexes = (inRange (width,height) (x,y) 1)
        withinOne <- (\bools -> (9-length oneIndexes) + length (filter id bools)) <$> mapM (VM.read src) oneIndexes
        let twoIndexes = (inRange (width,height) (x,y) 2)
        withinTwo <- (\bools -> (25-length twoIndexes) + length (filter id bools)) <$> mapM (VM.read src) (inRange (width,height) (x,y) 2)
        VM.write dst (x+y*width) (withinOne>=5 || withinTwo <=2)

caveCopy' :: PrimMonad m => Int -> Int -> VM.MVector (PrimState m) Bool -> VM.MVector (PrimState m) Bool -> m ()
caveCopy' width height src dst = do
    forM_ [(x,y) | x <- [0 .. width-1], y <- [0 .. height-1]] $ \(x,y) -> do
        let oneIndexes = (inRange (width,height) (x,y) 1)
        withinOne <- (\bools -> (9-length oneIndexes) + length (filter id bools)) <$> mapM (VM.read src) oneIndexes
        --withinTwo <- (length . filter id) <$> mapM (VM.read src) (inRange (width,height) (x,y) 2)
        VM.write dst (x+y*width) (withinOne>=5)
