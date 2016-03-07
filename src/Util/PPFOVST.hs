

{-| Computes the field of view that's visible from a given location. Use
'computeFOV' and supply a 'VisionBlocked' function, the computation range, and
the starting 'Location'. You get a (Set Location) back.

You can also use 'isLOSBetween' with a VisionBlocked and two Locations to get a
Bool of if vision is blocked between the locations or not. It will generally be
a cheaper computation than doing a full FOV calc, if you care about that sort of
micro-optimization.
-}
module Util.PPFOVST (
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
computeFOV visionB range start = Set.unions $ map (checkQuadrant visionB range start) [One .. Four]

{-| Untested, but should be correct. If this is the only thing you want to know,
then this should end up being faster than a full FOV.
-}
isLOSBetween :: VisionBlocked -> Location -> Location -> Bool
isLOSBetween vision locA locB =
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

-- -- --
-- Here begins the module-internal functions.
-- -- --

-- | Given an index, new value, and list, returns a list that
--   has the new value inserted immediately before the index given.
add :: Int -> a -> [a] -> [a]
add i new list@(x:xs)
    | i <  0 = error $ "Can't add at a ngative index: " ++ show i
    | i == 0 = new : list
    | otherwise = x : add (i-1) new xs
add 0 new [] = [new]
add i _   [] = error $ "Can't add into an empty list at index " ++ show i

-- | Given an index, new value, and a list, returns a list that
--   has the new value at the index given instead.
update :: Int -> a -> [a] -> [a]
update i new (x:xs) 
    | i <  0 = error $ "Can't update a negative index: " ++ show i
    | i == 0 = new : xs
    | otherwise = x : update (i-1) new xs
update 0 _ [] = []
update i _ [] = error $ "Can't update an empty list at index " ++ show i

-- | Remove the index specified from the list. If the index given
--   is greater than the length of the list there's no error.
remove :: Int -> [a] -> [a]
remove i (x:xs)
    | i <  0 = error $ "Can't remove from a negative index! " ++ show i
    | i == 0 = xs
    | otherwise = x : remove (i-1) xs
remove i [] = error $ "Can't index " ++ show i ++" from an empty list!"

-- | A sight line within a view. Has some associated geometry functions.
newtype SightLine = SightLine (Location,Location) deriving (Eq, Show)

-- | The X of the initial Location of a SightLine
getXInitial :: SightLine -> Int
getXInitial (SightLine (initial,_)) = getLocX initial

-- | The Y of the initial Location of a SightLine
getYInitial :: SightLine -> Int
getYInitial (SightLine (initial,_)) = getLocY initial

-- | The X of the final Location of a SightLine
getXFinal :: SightLine -> Int
getXFinal (SightLine (_,final)) = getLocX final

-- | The Y of the final Location of a SightLine
getYFinal :: SightLine -> Int
getYFinal (SightLine (_,final)) = getLocY final

-- | The initial location of the SightLine
getInitial :: SightLine -> Location
getInitial (SightLine (initial,_)) = initial

-- | The final location of the SightLine
getFinal :: SightLine -> Location
getFinal (SightLine (_,final)) = final

-- | Makes a new SightLine given an xInitial, yInitial, xFinal, and yFinal
mkSightLine :: Int -> Int -> Int -> Int -> SightLine
mkSightLine xi yi xf yf = SightLine (strictLocation xi yi, strictLocation xf yf)

-- | The relative slope between a SightLine and a Location
relativeSlope :: SightLine -> Location -> Int
relativeSlope line loc = (dy * (xf - x)) - (dx * (yf - y))
    where x = getLocX loc
          y = getLocY loc
          xf = getXFinal line
          yf = getYFinal line
          dx = xf - getXInitial line
          dy = yf - getYInitial line

-- | If the SightLine is entirely above the Location or not.
isAbove :: SightLine -> Location -> Bool
isAbove line loc = relativeSlope line loc < 0

-- | If the SightLine is above or touching the Location, or not.
isAboveOrCollinear :: SightLine -> Location -> Bool
isAboveOrCollinear line loc = relativeSlope line loc <= 0

-- | If the SightLine is entirely below the Location or not.
isBelow :: SightLine -> Location -> Bool
isBelow line loc = relativeSlope line loc > 0

-- | If the SightLine is below or touching the Locaiton, or not.
isBelowOrCollinear :: SightLine -> Location -> Bool
isBelowOrCollinear line loc = relativeSlope line loc >= 0

-- | If the SightLine is touching the Location or not.
isCollinear :: SightLine -> Location -> Bool
isCollinear line loc = relativeSlope line loc == 0

-- | If two lines are exactly aligned or not.
isLineCollinear :: SightLine -> SightLine -> Bool
isLineCollinear lineA lineB = (lineA `isCollinear` getInitial lineB) && (lineA `isCollinear` getFinal lineB)

-- | A View within the FOV process.
data View = View {
    getShallowBumps :: [Location],
    getShallowLine :: SightLine,
    getSteepBumps :: [Location],
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

-- | Reprisents the four quadrants of a cartesian grid.
data Quadrant = One
              | Two
              | Three
              | Four
              deriving (Show, Eq, Ord, Enum)

-- | Given a Quadrant, returns the signum of X coordinates in that Quadrant.
getSignX :: Quadrant -> Int
getSignX One   =  1
getSignX Two   = -1
getSignX Three = -1
getSignX Four  =  1

-- | Given a Quadrant, returns the signum of Y coordinates in that Quadrant.
getSignY :: Quadrant -> Int
getSignY One   =  1
getSignY Two   =  1
getSignY Three = -1
getSignY Four  = -1

-- | Constructs a Quadrant from two Int values, based on their sign.
mkQuadrant :: Int -> Int -> Quadrant
mkQuadrant x y = if x > 0
    then if y > 0
            then One
            else Four
    else if y > 0
            then Two
            else Three

-- | Performs view calculations on a single Quadrant relative to the start
--   position of the overall FOV computation.
checkQuadrant :: VisionBlocked -> Int -> Location -> Quadrant -> Set Location
checkQuadrant visionB range start quadrant = let
    shallowLineStart = mkSightLine 0 1 range 0
    steepLineStart   = mkSightLine 1 0 0 range
    startViewList    = [mkView shallowLineStart steepLineStart]
    coordsToCheck    = coordsFromRange range
    in runST $ do
        viewsRef <- newSTRef startViewList
        visitedRef <- newSTRef (Set.singleton start)
        checkSub visionB start quadrant coordsToCheck visitedRef viewsRef
        readSTRef visitedRef

{- During each subfunction pass, if there are no more active views we halt
and return the set of visited coordinates so far. Otherwise we use visitCoord
to compute the next pass, giving us a new set of visited coordinates and
a new list of active views. -}
checkSub :: VisionBlocked -> Location -> Quadrant -> [Location] -> STRef s (Set Location) -> STRef s [View] -> ST s ()
checkSub visionB start quadrant [] visitedRef viewsRef = return ()
checkSub visionB start quadrant (t:ts) visitedRef viewsRef = do
    views <- readSTRef viewsRef
    unless (null views) $ do
        let dx = getLocX t
        let dy = getLocY t
        visitCoord visionB start quadrant dx dy visitedRef viewsRef
        checkSub visionB start quadrant ts visitedRef viewsRef

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

-- TODO: Make these into Location -> View -> View funcs

-- | Adds the Location into the View as a new shallow bump, updating
--   the rest of the View as required.
addShallowBump :: View -> Location -> View
addShallowBump view loc = view {getShallowBumps = newShallowBumps, getShallowLine = newShallowLine}
    where newShallowBumps = loc : getShallowBumps view
          newShallowLine = foldl (nextLine isAbove) (SightLine (getInitial (getShallowLine view),loc)) (getSteepBumps view)

-- | Adds the Location into the View as a new steep bump, updating
--   the rest of the View as required.
addSteepBump :: View -> Location -> View
addSteepBump view loc = view {getSteepBumps = newSteepBumps, getSteepLine = newSteepLine}
    where newSteepBumps = loc : getSteepBumps view
          newSteepLine = foldl (nextLine isBelow) (SightLine (getInitial (getSteepLine view),loc)) (getShallowBumps view)

-- | Computes the next line when adding a shallow or steep bump.
nextLine :: (SightLine -> Location -> Bool) -> SightLine -> Location -> SightLine
nextLine pred line@(SightLine (i,f)) loc = if line `pred` loc
    then SightLine (loc,f)
    else line

{-| A View gets adjusted to be thinner and thinner as it's bumped by shallow
and steep bumps. A View remains valid until both of its lines are collinear
and they pass through either extremity of the origin square, (0,1) or (1,0).
Those corners can't be used as the sole projection point for a View.
-}
validView :: View -> Bool
validView view = not (shallowIsSteep && lineOnExtremity)
    where shallowIsSteep = shallowLine `isLineCollinear` steepLine
          lineOnExtremity = shallowLine `isCollinear` strictLocation 0 1 || shallowLine `isCollinear` strictLocation 1 0
          shallowLine = getShallowLine view
          steepLine = getSteepLine view

-- | Applies a bump operation, checks, and returns the new view list as a single operation.
bumpAndCheck :: (View -> Location -> View) -> Int -> Location -> [View] -> [View]
bumpAndCheck bumpf viewIndex bump activeViews = out
    where view = activeViews !! viewIndex
          bumpedView = bumpf view bump
          out = if validView bumpedView
            then update viewIndex bumpedView activeViews
            else remove viewIndex activeViews

-- | Adds a single Location to the set of visited locations if it's within
--   one of the views, and updates any views as necessary.
visitCoord :: VisionBlocked -> Location -> Quadrant -> Int -> Int -> STRef s (Set Location) -> STRef s [View] -> ST s ()
visitCoord visionB start quadrant dx dy visitedRef viewsRef = do
    let topLeft = strictLocation dx (dy+1)
    let bottomRight = strictLocation (dx+1) dy
    let realX = dx * getSignX quadrant
    let realY = dy * getSignY quadrant
    let trueLocation = strictLocation (getLocX start + realX) (getLocY start + realY)
    activeViews <- readSTRef viewsRef
    let viewIndex = calcViewIndex activeViews bottomRight
    unless (viewIndex == (length activeViews) || (getShallowLine (activeViews !! viewIndex)) `isAboveOrCollinear` topLeft) $ do
        modifySTRef' visitedRef (Set.insert trueLocation) -- Add the location
        when (visionB trueLocation) $ do
            let currentView = activeViews !! viewIndex -- Vision is blocked, calculate how it affects the view.
            let shallowAboveBottomRight = (getShallowLine currentView) `isAbove` bottomRight
            let steepBelowTopLeft = (getSteepLine currentView) `isBelow` topLeft
            case (shallowAboveBottomRight, steepBelowTopLeft) of
                (True, True) -> modifySTRef' viewsRef $ remove viewIndex
                (True, False) -> modifySTRef' viewsRef $ bumpAndCheck addShallowBump viewIndex topLeft
                (False, True) -> modifySTRef' viewsRef $ bumpAndCheck addSteepBump viewIndex bottomRight
                (False, False) -> do
                    -- clone the current view
                    modifySTRef' viewsRef $ add viewIndex currentView
                    -- check the shallow
                    -- we do this first so that if it's removed it won't affect our bump/check of the steep
                    modifySTRef' viewsRef $ bumpAndCheck addShallowBump (viewIndex+1) topLeft
                    -- check the steep
                    modifySTRef' viewsRef $ bumpAndCheck addSteepBump viewIndex bottomRight

-- | Calculates the index within the list that the view appropriate to the
--   Location specified has. If no view is appropriate to the Location
--   specified then the number returned is the length of the list.
calcViewIndex :: [View] -> Location -> Int
calcViewIndex = go 0
    where go tmp views bottomRight = if tmp < (length views) && getSteepLine (views!!tmp) `isBelowOrCollinear` bottomRight
            then go (tmp+1) views bottomRight
            else tmp
