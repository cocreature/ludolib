{-# LANGUAGE MagicHash, BangPatterns #-}

{-| Computes the field of view that's visible from a given location. Use
'computeFOV' and supply a 'VisionBlocked' function, the computation range, and
the starting 'Location'. You get a (Set Location) back.

You can also use 'isLOSBetween' with a VisionBlocked and two Locations to get a
Bool of if vision is blocked between the locations or not. It will generally be
a cheaper computation than doing a full FOV calc, if you care about that sort of
micro-optimization.
-}
module Util.PPFOVNext (
    VisionBlocked,
    computeFOV,
    isLOSBetween
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Location

import Control.Monad
import Control.Monad.ST
import Data.STRef

import GHC.Prim
import GHC.Exts

{-| A VisionBlocked is a function that, given a Location, says if that Location
blocks vision or not. Each such function is naturally specific to a particular
form of sight, and a given 2D plane. A check for x-ray vision and for normal
vision would be different, for example.  Should return True when vision is
blocked, and False when not.
-}
type VisionBlocked = Location -> Bool

{-| Given a vision form, a range limit, and a starting Location, returns the Set
of all Locations that can be seen.
-}
computeFOV :: VisionBlocked -> Int -> Location -> Set Location
computeFOV visionB range start = Set.unions $ [
    checkQuadrant visionB range start 1# 1#,
    checkQuadrant visionB range start 1# (-1#),
    checkQuadrant visionB range start (-1#) 1#,
    checkQuadrant visionB range start (-1#) (-1#)]


{-| Untested, but should be correct. If this is the only thing you want to know,
then this should end up being faster than a full FOV.
-}
isLOSBetween :: VisionBlocked -> Location -> Location -> Bool
isLOSBetween vision locA locB = undefined {- TODO: fix this once the rest is settled.
    let manhattan = max (abs $ getLocX locA - getLocX locB) (abs $ getLocY locA - getLocY locA)
    -- TODO: this can probably be done with mkQuadrant by subtracting the A points from the B points.
        in case getLocX locA `compare` getLocX locB of
            LT -> case getLocY locA `compare` getLocY locB of
                LT -> Set.member locB (checkQuadrant vision manhattan locA One) -- a.x < b.x && a.y < b.y
                EQ -> Set.member locB (checkQuadrant vision manhattan locA One) -- a.x < b.x && a.y == b.y
                GT -> Set.member locB (checkQuadrant vision manhattan locA Four) -- a.x < b.x && a.y > b.y
            EQ -> case getLocY locA `compare` getLocY locB of
                LT -> Set.member locB (checkQuadrant vision manhattan locA One) -- a.x == b.x && a.y < b.y
                EQ -> True -- locA is locB, no need to compute anything.
                GT -> Set.member locB (checkQuadrant vision manhattan locA Four) -- a.x == b.x && a.y > b.y
            GT -> case getLocY locA `compare` getLocY locB of
                LT -> Set.member locB (checkQuadrant vision manhattan locA Two) -- a.x > b.x && a.y < b.y
                EQ -> Set.member locB (checkQuadrant vision manhattan locA Two) -- a.x > b.x && a.y == b.y
                GT -> Set.member locB (checkQuadrant vision manhattan locA Three) -- a.x > b.x && a.y > b.y
-}

-- -- --
-- Here begins the module-internal functions.
-- -- --

indexError = error "Index error."

-- | Given an index, new value, and list, returns a list that
--   has the new value inserted immediately before the index given.
add :: Int# -> a -> [a] -> [a]
add i new list
    | isTrue# (i ># 0#) = case list of
        (x:xs) -> x : add (i -# 1#) new xs
        [] -> indexError
    | isTrue# (i ==# 0#) = new : list
    | otherwise = indexError

-- | Given an index, new value, and a list, returns a list that
--   has the new value at the index given instead.
update :: Int# -> a -> [a] -> [a]
update i new list
    | isTrue# (i ># 0#) = case list of
        (x:xs) -> x : update (i -# 1#) new xs
        [] -> indexError
    | isTrue# (i ==# 0#) = case list of
        (x:xs) -> new : xs
        [] -> indexError
    | otherwise = indexError

-- | Remove the index specified from the list. If the index given
--   is greater than the length of the list there's no error.
remove :: Int# -> [a] -> [a]
remove i list
    | isTrue# (i ># 0#) = case list of
        (x:xs) -> x : remove (i -# 1#) xs
        [] -> indexError
    | isTrue# (i ==# 0#) = case list of
        (x:xs) -> xs
        [] -> indexError
    | otherwise = indexError

infixr 1 #!
(#!) :: [a] -> Int# -> a
list #! i
    | isTrue# (i ># 0#) = (tail list) #! (i -# 1#)
    | isTrue# (i ==# 0#) = head list
    | otherwise = error "Negative Index."

ubLen :: [a] -> Int#
ubLen = go 0#
    where go !acc [] = acc
          go !acc (x:xs) = go (acc +# 1#) xs

-- | A sight line within a view. Has some associated geometry functions.
data SightLine = SightLine Int# Int# Int# Int# deriving (Eq, Show)

-- | The X of the initial Location of a SightLine
getXInitial :: SightLine -> Int#
getXInitial (SightLine xi yi xf yf) = xi

-- | The Y of the initial Location of a SightLine
getYInitial :: SightLine -> Int#
getYInitial (SightLine xi yi xf yf) = yi

-- | The X of the final Location of a SightLine
getXFinal :: SightLine -> Int#
getXFinal (SightLine xi yi xf yf) = xf

-- | The Y of the final Location of a SightLine
getYFinal :: SightLine -> Int#
getYFinal (SightLine xi yi xf yf) = yf

-- | Makes a new SightLine given an xInitial, yInitial, xFinal, and yFinal
mkSightLine :: Int# -> Int# -> Int# -> Int# -> SightLine
mkSightLine xi yi xf yf = SightLine xi yi xf yf

-- | The relative slope between a SightLine and a Location
relativeSlope :: SightLine -> Int# -> Int# -> Int#
relativeSlope (SightLine xi yi xf yf) lx ly = ((yf -# yi) *# (xf -# lx)) -# ((xf -# xi) *# (yf -# lx))

-- | If the SightLine is entirely above the Location or not.
isAbove :: SightLine -> Int# -> Int# -> Bool
isAbove line lx ly = isTrue# (relativeSlope line lx ly <# 0#)

-- | If the SightLine is above or touching the Location, or not.
isAboveOrCollinear :: SightLine -> Int# -> Int# -> Bool
isAboveOrCollinear line lx ly = isTrue# (relativeSlope line lx ly <=# 0#)

-- | If the SightLine is entirely below the Location or not.
isBelow :: SightLine -> Int# -> Int# -> Bool
isBelow line lx ly = isTrue# (relativeSlope line lx ly ># 0#)

-- | If the SightLine is below or touching the Locaiton, or not.
isBelowOrCollinear :: SightLine -> Int# -> Int# -> Bool
isBelowOrCollinear line lx ly = isTrue# (relativeSlope line lx ly >=# 0#)

-- | If the SightLine is touching the Location or not.
isCollinear :: SightLine -> Int# -> Int# -> Bool
isCollinear line lx ly = isTrue# (relativeSlope line lx ly ==# 0#)

-- | If two lines are exactly aligned or not.
isLineCollinear :: SightLine -> SightLine -> Bool
isLineCollinear lineA (SightLine xi yi xf yf) = (isCollinear lineA xi yi) && (isCollinear lineA xf yf)

data ViewBump = ViewBump Int# Int# deriving (Eq,Show)

-- | A View within the FOV process.
data View = View {
    getShallowBumps :: [ViewBump],
    getShallowLine :: SightLine,
    getSteepBumps :: [ViewBump],
    getSteepLine :: SightLine
    } deriving (Eq, Show)

-- | Makes a new View from the shallow and steep line given.
--   The bump lists start out empty.
mkView :: SightLine -> SightLine -> View
mkView shallowLine steepLine = View {
    getShallowBumps=[],
    getShallowLine=shallowLine,
    getSteepBumps=[],
    getSteepLine=steepLine}

boxLocation :: Int# -> Int# -> Location
boxLocation x y = strictLocation (I# x) (I# y)

-- | Performs view calculations on a single Quadrant relative to the start
--   position of the overall FOV computation.
-- TODO: Eliminate the Quadrant type entirely and just accept two ints/bools here.
checkQuadrant :: VisionBlocked -> Int -> Location -> Int# -> Int# -> Set Location
checkQuadrant visionB range start qx qy = let
    !(I# ubRange)    = range
    shallowLineStart = mkSightLine 0# 1# ubRange 0#
    steepLineStart   = mkSightLine 1# 0# 0# ubRange
    startViewList    = [mkView shallowLineStart steepLineStart]
    coordsToCheck    = coordsFromRange range
    !(I# startX)     = getLocX start
    !(I# startY)     = getLocY start
    in runST $ do
        viewsRef <- newSTRef startViewList
        visitedRef <- newSTRef (Set.singleton start) -- TODO: This should be a list that we just prepend to.
        checkSub visionB startX startY qx qy coordsToCheck visitedRef viewsRef
        readSTRef visitedRef

{- During each subfunction pass, if there are no more active views we halt
and return the set of visited coordinates so far. Otherwise we use visitCoord
to compute the next pass, giving us a new set of visited coordinates and
a new list of active views. -}
checkSub :: VisionBlocked -> Int# -> Int# -> Int# -> Int# -> [Location] -> STRef s (Set Location) -> STRef s [View] -> ST s ()
checkSub visionB startX startY qx qy [] visitedRef viewsRef = return ()
checkSub visionB startX startY qx qy (t:ts) visitedRef viewsRef = do
    views <- readSTRef viewsRef
    unless (null views) $ do
        let !(I# dx) = getLocX t
            !(I# dy) = getLocY t
        visitCoord visionB startX startY qx qy dx dy visitedRef viewsRef
        checkSub visionB startX startY qx qy ts visitedRef viewsRef

-- | Turns a range for the vision into a list of the locations, relative
--   to the start position, that should be checked per quadrant.
coordsFromRange :: Int -> [Location]
coordsFromRange range = do
    let maxIndex = (2*range) + 1
    i <- [1..(maxIndex-1)]
    let startJ = max (i-range) 0
    let maxJ = min i range + 1
    j <- [startJ..(maxJ-1)]
    return $ strictLocation (i-j) j

-- | Adds the Location into the View as a new shallow bump, updating
--   the rest of the View as required.
addShallowBump :: Int# -> Int# -> View -> View
addShallowBump lx ly view = view {getShallowBumps = newShallowBumps, getShallowLine = newShallowLine}
    where newShallowBumps = ViewBump lx ly : getShallowBumps view
          !(SightLine oxi oyi oxf oyf) = getShallowLine view
          newShallowLine = foldl (nextLine isAbove) (SightLine oxi oyi lx ly) (getSteepBumps view)

-- | Adds the Location into the View as a new steep bump, updating
--   the rest of the View as required.
addSteepBump :: Int# -> Int# -> View -> View
addSteepBump lx ly view = view {getSteepBumps = newSteepBumps, getSteepLine = newSteepLine}
    where newSteepBumps = ViewBump lx ly : getSteepBumps view
          !(SightLine oxi oyi oxf oyf) = getSteepLine view
          newSteepLine = foldl (nextLine isBelow) (SightLine oxi oyi lx ly) (getShallowBumps view)

-- | Computes the next line when adding a shallow or steep bump.
nextLine :: (SightLine -> Int# -> Int# -> Bool) -> SightLine -> ViewBump -> SightLine
nextLine pred !line@(SightLine xi yi xf yf) !(ViewBump vx vy) = if pred line vx vy
    then SightLine vx vy xf yf
    else line

{-| A View gets adjusted to be thinner and thinner as it's bumped by shallow
and steep bumps. A View remains valid until both of its lines are collinear
and they pass through either extremity of the origin square, (0,1) or (1,0).
Those corners can't be used as the sole projection point for a View.
-}
validView :: View -> Bool
validView view = not (shallowIsSteep && lineOnExtremity)
    where shallowIsSteep = shallowLine `isLineCollinear` steepLine
          lineOnExtremity = isCollinear shallowLine 0# 1# || isCollinear shallowLine 1# 0#
          shallowLine = getShallowLine view
          steepLine = getSteepLine view

-- | Applies a bump operation, checks, and returns the new view list as a single operation.
bumpAndCheck :: (View -> View) -> Int# -> [View] -> [View]
bumpAndCheck bumpf viewIndex activeViews = out
    where view = activeViews #! viewIndex
          bumpedView = bumpf view
          out = if validView bumpedView
            then update viewIndex bumpedView activeViews
            else remove viewIndex activeViews

-- | Adds a single Location to the set of visited locations if it's within
--   one of the views, and updates any views as necessary.
visitCoord :: VisionBlocked -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> STRef s (Set Location) -> STRef s [View] -> ST s ()
visitCoord visionB startX startY qx qy dx dy visitedRef viewsRef = do
    let topLeftX = dx
        topLeftY = dy +# 1#
        bottomRightX = dx +# 1#
        bottomRightY = dy
        realX = dx *# qx -- only keeps the low word, i guess.
        realY = dy *# qy
        trueLocation = strictLocation (I# (startX +# realX)) (I# (startY +# realY))
    activeViews <- readSTRef viewsRef
    let viewIndex = (calcViewIndex activeViews bottomRightX bottomRightY) :: Int#
    unless (isTrue# (viewIndex ==# ubLen activeViews) || isAboveOrCollinear (getShallowLine (activeViews #! viewIndex)) topLeftX topLeftY) $ do
        modifySTRef' visitedRef (Set.insert trueLocation) -- Add the location
        when (visionB trueLocation) $ do
            let currentView = activeViews #! viewIndex -- Vision is blocked, calculate how it affects the view.
                shallowAboveBottomRight = isAbove (getShallowLine currentView) bottomRightX bottomRightY
                steepBelowTopLeft = isBelow (getSteepLine currentView) topLeftX topLeftY
            case (shallowAboveBottomRight, steepBelowTopLeft) of
                (True, True) -> modifySTRef' viewsRef $ remove viewIndex
                (True, False) -> modifySTRef' viewsRef $ bumpAndCheck (addShallowBump topLeftX topLeftY) viewIndex 
                (False, True) -> modifySTRef' viewsRef $ bumpAndCheck (addSteepBump bottomRightX bottomRightY) viewIndex 
                (False, False) -> do
                    -- clone the current view
                    modifySTRef' viewsRef $ add viewIndex currentView
                    -- check the shallow
                    -- we do this first so that if it's removed it won't affect our bump/check of the steep
                    modifySTRef' viewsRef $ bumpAndCheck (addShallowBump topLeftX topLeftY) (viewIndex +# 1#) 
                    -- check the steep
                    modifySTRef' viewsRef $ bumpAndCheck (addSteepBump bottomRightX bottomRightY) viewIndex

-- | Calculates the index within the list that the view appropriate to the
--   Location specified has. If no view is appropriate to the Location
--   specified then the number returned is the length of the list.
calcViewIndex :: [View] -> Int# -> Int# -> Int#
calcViewIndex = go 0#
    where go :: Int# -> [View] -> Int# -> Int# -> Int#
          go tmp views bottomRightX bottomRightY = if isTrue# (tmp <# ubLen views) && isBelowOrCollinear (getSteepLine (views #! tmp)) bottomRightX bottomRightY
            then go (tmp +# 1#) views bottomRightX bottomRightY
            else tmp

-- isBelowOrCollinear :: SightLine -> Int# -> Int# -> Bool
