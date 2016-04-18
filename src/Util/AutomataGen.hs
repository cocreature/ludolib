
{-| Makes dungeons via a cellular automata style technique.

INCOMPLETE AT THE MOMENT
-}
module Util.AutomataGen (
    SimpleDungeon(..),
    hasWall,
    formatSimple,
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
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Bool
import Data.Location
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as S
import Data.STRef

{-| There's a lot of features a dungeon can have in a game, but for cellular
automata work we just need to know what space we're working with and if there's
a wall or not in each cell.
-}
data SimpleDungeon = SimpleDungeon {
    _getWidth :: !Int, -- ^ the width of this dungeon
    _getHeight :: !Int, -- ^ the height of this dungeon
    _getWalls :: !(Vector Bool)
    } deriving (Eq, Ord, Show)

{-| Turns a 'SimpleDungeon' into a String suitable for use with 'putStrLn'
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

{-| @fromIndex index width@, Turns an index into a location
-}
fromIndex :: Int -> Int -> Location
fromIndex width index = Location $ swap $ divMod index width
{-# INLINE fromIndex #-}

{-| @toIndex width (x,y)@, Turns a location into an index
-}
toIndex :: Int -> Location -> Int
toIndex width (Location (x,y)) = (x + y*width)
{-# INLINE toIndex #-}

{-| If the location specified has a wall. Out of bounds locations default to
True so that players don't walk off the edge.
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

{-| Makes a dungeon that's all floor except for an outline of walls around the
edges.
-}
mkOutline :: Int -> Int -> SimpleDungeon
mkOutline width height = SimpleDungeon width height $ runST $ do
    d <- VM.unsafeNew (width*height) 
    forM_ [(x,y) | x <- [0 .. width-1], y <- [0 .. height-1]] $ \(x,y) ->
        VM.write d (toIndex width (Location (x,y))) (x == 0 || y == 0 || x == width-1 || y == height-1)
    V.unsafeFreeze d

{-| @mkRandom width height chance@, Makes a @width*height@ length Vector Bool
where each cell has a @chance@ percent chance of being True. This lets you
have a field of random bushes or rocks or something.
-}
mkRandom :: MonadRandom m => Int -> Int -> Int -> m (Vector Bool)
mkRandom width height chance = V.replicateM (width*height) (rollChance chance)

{-| @mkCaves width height@, Produces a SimpleDungeon based on a cellular
automata technique. Portions not connected to the main cave at the end are
automatically filled in for you.
-}
mkCaves :: MonadRandom m => Int -> Int -> m SimpleDungeon
mkCaves width height = do
    let len = width * height
    -- using Word gives better locality than using Bool, I guess.
    base <- V.replicateM len ((bool (0::Word) 1) <$> (rollChance (40::Word)))
    let autoResult = runST $ do
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
                    -- We set the destination to be all walls.
                    mapM_ (\i -> VM.unsafeWrite foo i 1) [0 .. len -1]
                    -- Copy the open space into it
                    closedSet <- newSTRef (S.empty)
                    floodCopy8 width height bar foo closedSet i
                    Just <$> V.unsafeFreeze foo
    case autoResult of
        -- somehow there's no floor anywhere, so we'll just try again. the RNG moved, so it'll be okay.
        Nothing -> mkCaves width height
        -- success!
        Just wordVec -> return $ SimpleDungeon width height (V.map (>0) wordVec)

{-| @caveCopy width height src dst@, Copies info from src to dst, not directly,
but based on a cellular automata style system.
-}
caveCopy :: PrimMonad m => Int -> Int -> VM.MVector (PrimState m) Word -> VM.MVector (PrimState m) Word -> m ()
caveCopy width height src dst = do
    forM_ [(x,y) | x <- [0 .. width-1], y <- [0 .. height-1]] $ \(x,y) -> do
        let oneIndexes = toIndex width (Location (x,y)) : (atRange1 (width,height) (Location (x,y)))
        withinOne <- (\words -> (9-length oneIndexes) + length (filter (==1) words)) <$> mapM (VM.unsafeRead src) oneIndexes
        if withinOne >= 5
            then VM.unsafeWrite dst (x+y*width) 1
            else do
                let twoIndexes = (atRange2 (width,height) (Location (x,y)))
                withinTwo <- (\words -> (25-length twoIndexes) + length (filter (==1) words) + withinOne) <$> mapM (VM.unsafeRead src) twoIndexes
                VM.unsafeWrite dst (toIndex width (Location (x,y))) (if (withinTwo <=2) then 1 else 0)

{-| Finds an open space inside the vector specified, if possible. Starts in the
middleish.
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

{-| @floodCopy8 width height src dst closedSet i@, if @i@ is a floor and not in
the closed set, adds it to the closed set and writes that location to be floor
in @dst@, then spreads out recursively in all 8 directions. If @i@ is in the
closed set it stops that branch of the recusive descent.
-}
floodCopy8 :: Int -> Int -> VM.MVector s Word -> VM.MVector s Word -> STRef s (Set Int) -> Int -> ST s ()
floodCopy8 width height src dst closedSet i = do
    already <- (S.member i) <$> readSTRef closedSet
    isFloor <- (==0) <$> VM.unsafeRead src i
    -- true = wall, so we copy and spread out when we get a false
    if isFloor && not already
        then do
            modifySTRef' closedSet (S.insert i)
            VM.unsafeWrite dst i 0
            let indexes = atRange1 (width,height) (fromIndex width i)
            mapM_ (floodCopy8 width height src dst closedSet) indexes
        else return ()

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
