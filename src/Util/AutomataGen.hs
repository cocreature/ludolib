
{-| Makes dungeons via a cellular automata style technique.

INCOMPLETE AT THE MOMENT
-}
module Util.AutomataGen (
    SimpleDungeon(..),
    formatSimple,
    hasWall,
    mkDungeon,
    mkOutline,
    mkRandom,
    mkCaves
    ) where

import Control.Monad.ST
import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.RNG

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Bool
import Data.Location
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as S
import Data.STRef

{-| There's a lot of features a dungeon can have in a game, but for the simplest
work we just need to know where the walls are and where the floors are.
-}
data SimpleDungeon = SimpleDungeon {
    _getWidth :: !Int,
    _getHeight :: !Int,
    _getWalls :: !(Vector Bool)
    } deriving (Eq, Ord, Show)

{-| Turns a 'SimpleDungeon' into a String suitable for use with 'putStrLn'.
-}
formatSimple :: SimpleDungeon -> String
formatSimple d = let
    width = _getWidth d
    height = _getHeight d
    toChar = bool '.' '#'
    chars = map toChar (V.toList $ _getWalls d)
    groupsOf :: Int -> [a] -> [[a]]
    groupsOf x [] = []
    groupsOf x elems = take x elems : groupsOf x (drop x elems)
    in unlines $ groupsOf width chars

{-| Simplies the process of checking for a wall. Out of bounds locations default
to 'True', which makes this easy to use with Pathfinding and FOV.
-}
hasWall :: SimpleDungeon -> Location -> Bool
hasWall d loc = let
    x = getLocX loc
    y = getLocY loc
    width = _getWidth d
    height = _getHeight d
    cells = _getWalls d
    index = toIndex width (Location (x,y))
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
            VM.write d (toIndex width (Location (x,y))) True
    V.unsafeFreeze d

{-| @mkRandom width height chance@, Makes a SimpleDungeon where each cell has a
@chance@ percent chance of being True. This lets you have a field of bushes or
trees or whatever.
-}
mkRandom :: MonadRandom m => Int -> Int -> Int -> m SimpleDungeon
mkRandom width height chance = SimpleDungeon width height <$> V.replicateM (width*height) (rollChance chance)

{-| @mkCaves width height@, Produces a SimpleDungeon based on a cellular
automata technique. Automatically fills in any disconnected regions after the
automata process. If there's no open space after the main generation then it
will fail. This usually only happens when the map size is excessively small,
so just stick to maps that are at least 10 in both dimensions. The results of
this dungeon generator are usually a lot of twisty narrow tunnels that
interconnect all over.
-}
mkCaves :: MonadRandom m => Int -> Int -> m (Maybe SimpleDungeon)
mkCaves width height = do
    let len = width * height
    base <- V.replicateM len ((bool (0::Word) 1) <$> rollChance (40::Word))
    return . fmap (SimpleDungeon width height . V.map (>0)) $ runST $ do
        foo <- V.unsafeThaw base
        bar <- VM.unsafeNew len
        caveCopy width height foo bar
        caveCopy width height bar foo
        caveCopy width height foo bar
        caveCopy width height bar foo
        caveCopy width height foo bar
        maybeLoc <- findOpen bar
        case maybeLoc of
            Nothing -> return Nothing
            Just i -> do
                -- We set the destination space to be all walls.
                mapM_ (\i -> VM.unsafeWrite foo i 1) [0 .. len -1]
                -- Then copy the open space into it.
                closedSet <- newSTRef (S.empty)
                floodCopy8 width height bar foo closedSet i
                Just <$> V.unsafeFreeze foo

{-| Finds an open space, if possible. Starts around the middle of the vector,
which hopefully avoids picking any small disconnected spaces that sometimes
appear at the edges of a map.
-}
findOpen :: PrimMonad m => VM.MVector (PrimState m) Word -> m (Maybe Int)
findOpen vec = do
    let mid = VM.length vec `div` 2
        indexes = [0 .. VM.length vec -1] :: [Int]
        toCheck = drop mid indexes ++ take mid indexes -- this starts us in the middleish
        go [] = return Nothing
        go (x:xs) = do
            isFloor <- (==0) <$> VM.unsafeRead vec x
            if isFloor then return (Just x) else go xs
    go toCheck

{-| @floodCopy8 width height src dst closedSet i@ performs a copy from src to
dst that floods out from the initial index given in all 8 directions.
-}
floodCopy8 :: Int -> Int -> VM.MVector s Word -> VM.MVector s Word -> STRef s (Set Int) -> Int -> ST s ()
floodCopy8 width height src dst closedSet i = do
    -- have we been here before?
    notFloodedHere <- not.(S.member i) <$> readSTRef closedSet
    when (notFloodedHere) $ do
        -- If not, mark that we have now, and check for a floor
        modifySTRef' closedSet (S.insert i)
        hereIsFloor <- (==0) <$> VM.unsafeRead src i
        when (hereIsFloor) $ do
            -- if there's a floor, write that to dst and recurse
            VM.unsafeWrite dst i 0
            let loc = fromIndex width i
                indexes = atRange1 (width,height) loc
            mapM_ (floodCopy8 width height src dst closedSet) indexes

{-| @caveCopy width height src dst@, copies from src to dst, not directly, but
based on a cellular automata style system. If there's 5 or more cells active
within 1 square, the square is always active. Otherwise, if there's less than
2 cells active within 2 then the cell will always be active. Otherwise, the
new cell is the same as the old cell.
-}
caveCopy :: PrimMonad m => Int -> Int -> VM.MVector (PrimState m) Word -> VM.MVector (PrimState m) Word -> m ()
caveCopy width height src dst = do
    forM_ [(x,y) | x <- [0 .. width-1], y <- [0 .. height-1]] $ \(x,y) -> do
        let oneIndexes = toIndex width (Location (x,y)) : (atRange1 (width,height) (Location (x,y)))
        withinOne <- (\words -> (9-length oneIndexes) + length (filter (>0) words)) <$> mapM (VM.unsafeRead src) oneIndexes
        if withinOne >= 5
            then VM.unsafeWrite dst (toIndex width (Location (x,y))) 1
            else do
                let twoIndexes = (atRange2 (width,height) (Location (x,y)))
                withinTwo <- (\words -> (25-(length twoIndexes + length oneIndexes)) + length (filter (>0) words) + withinOne) <$> mapM (VM.unsafeRead src) twoIndexes
                VM.unsafeWrite dst (toIndex width (Location (x,y))) (if (withinTwo <=2) then 1 else 0)

{-| @fromIndex width index@, turns an index into a Location.
-}
fromIndex :: Int -> Int -> Location
fromIndex width index = Location $ swap $ divMod index width
{-# INLINE fromIndex #-}

{-| @toIndex width location@, Turns a Location into an index
-}
toIndex :: Int -> Location -> Int
toIndex width (Location (x,y)) = (x + y*width)
{-# INLINE toIndex #-}

{-| @atRange1 (width,height) (cx,cy)@, Gives the list of all indexes that are
exactly range 1 from the location given.
-}
atRange1 :: (Int,Int) -> Location -> [Int]
atRange1 (width,height) (Location (cx,cy)) = do
    (x,y) <- [(cx-1,cy-1),(cx-1,cy),(cx-1,cy+1),
              (cx,cy-1),              (cx,cy+1),
              (cx+1,cy-1),(cx+1,cy),(cx+1,cy+1)]
    guard $ not $ x < 0 || y < 0 || x >= width || y >= height
    return $ toIndex width (Location (x,y))

{-| @atRange2 (width,height) (cx,cy)@, Gives the list of all indexes that are
exactly 2 steps from the location given.
-}
atRange2 :: (Int,Int) -> Location -> [Int]
atRange2 (width,height) (Location (cx,cy)) = do
    (x,y) <- [(cx-2,cy-2),(cx-2,cy-1),(cx-2,cy),(cx-2,cy+1),(cx-2,cy+2),
              (cx-1,cy-2),                                  (cx-2,cx+2),
              (cx,cy-2),                                      (cx,cy+2),
              (cx+1,cy-2),                                  (cx+1,cy+2),
              (cx+2,cy-2),(cx+2,cy-1),(cx+2,cy),(cx+2,cy+1),(cx+2,cy+2)]
    guard $ not $ x < 0 || y < 0 || x >= width || y >= height
    return $ toIndex width (Location (x,y))
