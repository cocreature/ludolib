
{-| Makes dungeons via a cellular automata style technique.

INCOMPLETE AT THE MOMENT
-}
module Util.AutomataGen where

import Control.Monad.ST
import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.RNG

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as VM
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

{-| @ mkCaves width height @

Produces a SimpleDungeon based on a cellular automata technique.
-}
mkCaves :: MonadRandom m => Int -> Int -> m SimpleDungeon
mkCaves width height = do
    base <- mkRandom width height 40
    let autoResult = runST $ do
        foo <- V.thaw base
        bar <- VM.replicate (width*height) False
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
                mapM_ (\i -> VM.unsafeWrite foo i True) [0 .. (width*height) -1]
                -- Copy the open space into it
                closedSet <- newSTRef (S.empty)
                floodCopy8 width height bar foo closedSet i
                Just <$> V.unsafeFreeze foo
    case autoResult of
        Nothing -> mkCaves width height
        Just vec -> return $ SimpleDungeon width height vec

findOpen :: PrimMonad m => VM.MVector (PrimState m) Bool -> m (Maybe Int)
findOpen vec = do
    let mid = VM.length vec `div` 2
        indexes = [0 .. VM.length vec -1] :: [Int]
        toCheck = drop mid indexes ++ take mid indexes -- this starts us in the middleish
        go [] = return Nothing
        go (x:xs) = do
            isWall <- VM.unsafeRead vec x
            if not isWall then return (Just x) else go xs
    go toCheck

floodCopy8 :: Int -> Int -> VM.MVector s Bool -> VM.MVector s Bool -> STRef s (Set Int) -> Int -> ST s ()
floodCopy8 width height src dst closedSet i = do
    already <- (S.member i) <$> readSTRef closedSet
    wallHere <- VM.unsafeRead src i
    -- true = wall, so we copy and spread out when we get a false
    if not wallHere && not already
        then do
            modifySTRef' closedSet (S.insert i)
            VM.unsafeWrite dst i False
            let loc = fromIndex width i
                indexes = inRange (width,height) loc 1
            mapM_ (floodCopy8 width height src dst closedSet) indexes
            --return ()
        else return ()

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

{-| @ caveCopy width height src dst @

Copies info from src to dst, not directly, but based on a cellular automata
style system.
-}
caveCopy :: PrimMonad m => Int -> Int -> VM.MVector (PrimState m) Bool -> VM.MVector (PrimState m) Bool -> m ()
caveCopy width height src dst = do
    forM_ [(x,y) | x <- [0 .. width-1], y <- [0 .. height-1]] $ \(x,y) -> do
        let oneIndexes = (inRange (width,height) (x,y) 1)
        withinOne <- (\bools -> (9-length oneIndexes) + length (filter id bools)) <$> mapM (VM.unsafeRead src) oneIndexes
        if withinOne >= 5
            then VM.unsafeWrite dst (x+y*width) True
            else do
                let twoIndexes = (inRange (width,height) (x,y) 2)
                withinTwo <- (\bools -> (25-length twoIndexes) + length (filter id bools)) <$> mapM (VM.unsafeRead src) twoIndexes
                VM.unsafeWrite dst (x+y*width) (withinTwo <=2)

{-| @ fromIndex index width @

Turns an index into a location
-}
fromIndex :: Int -> Int -> (Int,Int)
fromIndex width index = swap $ divMod index width

{-| @ toIndex width (x,y) @

Turns a location into an index
-}
toIndex :: Int -> (Int,Int) -> Int
toIndex width (x,y) = (x + y*width)
