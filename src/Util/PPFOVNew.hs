

{-| Computes the field of view that's visible from a given location. Use
'computeFOV' and supply a 'VisionBlocked' function, the computation range, and
the starting 'Location'. You get a (Set Location) back.

You can also use 'isLOSBetween' with a VisionBlocked and two Locations to get a
Bool of if vision is blocked between the locations or not. It will generally be
a cheaper computation than doing a full FOV calc, if you care about that sort of
micro-optimization.
-}
module Util.PPFOVNew where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Location

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
computeFOV vision range start = unionMap (checkQuadrant vision range start) [One .. Four]

{-| Untested, but should be correct. If this is the only thing you want to know,
then this should end up being faster than a full FOV.
-}
isLOSBetween :: VisionBlocked -> Location -> Location -> Bool
isLOSBetween vision locA locB =
    let manhattan = (max (abs $ (getLocX locA)-(getLocX locB)) (abs $ (getLocY locA)-(getLocY locA)))
    -- TODO: this can probably be done with mkQuadrant by subtracting the A points from the B points.
        in case (getLocX locA) `compare` (getLocX locB) of
            LT -> case (getLocY locA) `compare` (getLocY locB) of
                LT -> Set.member locB (checkQuadrant vision manhattan locA One) -- a.x < b.x && a.y < b.y
                EQ -> Set.member locB (checkQuadrant vision manhattan locA One) -- a.x < b.x && a.y == b.y
                GT -> Set.member locB (checkQuadrant vision manhattan locA Four) -- a.x < b.x && a.y > b.y
            EQ -> case (getLocY locA) `compare` (getLocY locB) of
                LT -> Set.member locB (checkQuadrant vision manhattan locA One) -- a.x == b.x && a.y < b.y
                EQ -> True -- locA is locB, no need to compute anything.
                GT -> Set.member locB (checkQuadrant vision manhattan locA Four) -- a.x == b.x && a.y > b.y
            GT -> case (getLocY locA) `compare` (getLocY locB) of
                LT -> Set.member locB (checkQuadrant vision manhattan locA Two) -- a.x > b.x && a.y < b.y
                EQ -> Set.member locB (checkQuadrant vision manhattan locA Two) -- a.x > b.x && a.y == b.y
                GT -> Set.member locB (checkQuadrant vision manhattan locA Three) -- a.x > b.x && a.y > b.y

-- -- --
-- Here begins the module-internal functions.
-- -- --

-- | Given an index, list, and new value, returns a list that
--   has the new value inserted at the index given.
add :: (Integral a) => a -> [b] -> b -> [b]
add a (b:bs) new
    | a <  0 = b : bs
    | a == 0 = new : b : bs
    | otherwise = b : add (a-1) bs new
add 0 []     new = [new]
add _ []     _ = []

-- | Given an index, list, and new value, returns a list that
--   has the new value at the index given instead.
update :: (Integral a) => a -> [b] -> b -> [b]
update a (b:bs) new
    | a <  0 = b : bs
    | a == 0 = new : bs
    | otherwise = b : update (a-1) bs new
update _ []     _ = []

-- | Remove the index specified from the list. If the index given
--   is greater than the length of the list there's no error.
remove :: (Integral a) => a -> [b] -> [b]
remove a (b:bs)
    | a <  0 = b : bs
    | a == 0 = bs
    | otherwise = b : remove (a-1) bs
remove _ [] = []

-- | Maps the function given over the list given, and return a set
--   that is the union of all the result sets.
unionMap :: (Ord b) => (a -> Set b) -> [a] -> Set b
unionMap f list = Set.unions $ map f list

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
          dx = xf - (getXInitial line)
          dy = yf - (getYInitial line)

-- | If the SightLine is entirely above the Location or not.
isAbove :: SightLine -> Location -> Bool
isAbove line loc = (relativeSlope line loc) < 0

-- | If the SightLine is above or touching the Location, or not.
isAboveOrCollinear :: SightLine -> Location -> Bool
isAboveOrCollinear line loc = (relativeSlope line loc) <= 0

-- | If the SightLine is entirely below the Location or not.
isBelow :: SightLine -> Location -> Bool
isBelow line loc = (relativeSlope line loc) > 0

-- | If the SightLine is below or touching the Locaiton, or not.
isBelowOrCollinear :: SightLine -> Location -> Bool
isBelowOrCollinear line loc = (relativeSlope line loc) >= 0

-- | If the SightLine is touching the Location or not.
isCollinear :: SightLine -> Location -> Bool
isCollinear line loc = (relativeSlope line loc) == 0

-- | If two lines are exactly aligned or not.
isLineCollinear :: SightLine -> SightLine -> Bool
isLineCollinear lineA lineB = (lineA `isCollinear` (getInitial lineB)) && (lineA `isCollinear` (getFinal lineB))

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
checkQuadrant vision range start quadrant = checkSub coordsToCheck (Set.singleton start) startViewList
    where shallowLineStart = mkSightLine 0 1 range 0
          steepLineStart   = mkSightLine 1 0 0     range
          startViewList = [mkView shallowLineStart steepLineStart]
          coordsToCheck = coordsFromRange range
          {- During each subfunction pass, if there are no more active views we halt
          and return the set of visited coordinates so far. Otherwise we use visitCoord
          to compute the next pass, giving us a new set of visited coordinates and
          a new list of active views. -}
          checkSub :: [Location] -> Set Location -> [View] -> Set Location
          checkSub _      visited []          = visited
          checkSub []     visited _           = visited
          checkSub (c:cs) visited activeViews = checkSub cs newVisited newActiveViews
            where (newVisited,newActiveViews) = visitCoord visited start dx dy quadrant activeViews vision
                  dx = getLocX c
                  dy = getLocY c

-- | Turns a range for the vision into a list of the locations, relative
--   to the start position, that should be checked per quadrant.
coordsFromRange :: Int -> [Location]
coordsFromRange range = do
    let maxIndex = (2*range) + 1
    i <- [1..(maxIndex-1)]
    let startJ = max (i-range) 0
    let maxJ = (min i range) + 1
    j <- [startJ..(maxJ-1)]
    return $ (strictLocation (i-j) j)

-- | Adds the Location into the View as a new shallow bump, updating
--   the rest of the View as required.
addShallowBump :: View -> Location -> View
addShallowBump view loc = view {getShallowBumps = newShallowBumps, getShallowLine = newShallowLine}
    where newShallowBumps = loc : (getShallowBumps view)
          newShallowLine = foldl (nextLine isAbove) (SightLine (getInitial (getShallowLine view),loc)) (getSteepBumps view)

-- | Adds the Location into the View as a new steep bump, updating
--   the rest of the View as required.
addSteepBump :: View -> Location -> View
addSteepBump view loc = view {getSteepBumps = newSteepBumps, getSteepLine = newSteepLine}
    where newSteepBumps = loc : (getSteepBumps view)
          newSteepLine = foldl (nextLine isBelow) (SightLine (getInitial (getSteepLine view),loc)) (getShallowBumps view)

-- | Computes the next line when adding a shallow or steep bump.
nextLine :: (SightLine -> Location -> Bool) -> SightLine -> Location -> SightLine
nextLine pred line@(SightLine (i,f)) loc = if line `pred` loc
    then SightLine (loc,f)
    else line

{-
{-| Removes the view at the viewIndex specified from the activeViews if both
of the view's lines are collinear and they also pass through either extremity;
(0,1) and (1,0). The corners of the source square cannot be used as the origin
of a view, and so if this happens the view needs to be removed. This needs to be
called after every view update for all views.
-}
checkView :: [View] -> Int -> [View]
-- TODO: Remove this once it's confirmed to no longer be needed.
checkView activeViews viewIndex = if shallowIsSteep && lineOnExtremity
    then remove viewIndex activeViews
    else activeViews
        where shallowIsSteep = shallowLine `isLineCollinear` steepLine
              lineOnExtremity = (shallowLine `isCollinear` (strictLocation 0 1) || shallowLine `isCollinear` (strictLocation 1 0))
              shallowLine = getShallowLine (activeViews !! viewIndex)
              steepLine = getSteepLine (activeViews !! viewIndex)
-}

{-| A View gets adjusted to be thinner and thinner as it's bumped by shallow
and steep bumps. A View remains valid until both of its lines are collinear
and they pass through either extremity of the origin square, (0,1) or (1,0).
Those corners can't be used as the sole projection point for a View.
-}
validView :: View -> Bool
validView view = not (shallowIsSteep && lineOnExtremity)
    where shallowIsSteep = shallowLine `isLineCollinear` steepLine
          lineOnExtremity = (shallowLine `isCollinear` (strictLocation 0 1) || shallowLine `isCollinear` (strictLocation 1 0))
          shallowLine = getShallowLine view
          steepLine = getSteepLine view

-- | Applies a bump operation, checks, and returns the new view list as a single operation.
bumpAndCheck :: (View -> Location -> View) -> [View] -> Int -> Location -> [View]
bumpAndCheck bumpf activeViews viewIndex bump = out
    where view = activeViews !! viewIndex
          bumpedView = bumpf view bump
          out = if validView bumpedView
            then update viewIndex activeViews bumpedView
            else remove viewIndex activeViews

-- | Adds a single Location to the set of visited locations if it's within
--   one of the views, and updates any views as necessary.
visitCoord :: Set Location -> Location -> Int -> Int -> Quadrant -> [View] -> VisionBlocked -> (Set Location,[View])
visitCoord visited start dx dy quadrant activeViews vision =
    let topLeft = strictLocation dx (dy+1)
        bottomRight = strictLocation (dx+1) dy
        realX = dx * getSignX quadrant
        realY = dy * getSignY quadrant
        trueLocation = strictLocation (getLocX start + realX) (getLocY start + realY)
        viewIndex = calcViewIndex activeViews bottomRight
        -- let bindings below here could be floated up, but they're grouped near the if statement they relate to.
        in if viewIndex == (length activeViews) || (getShallowLine (activeViews !! viewIndex)) `isAboveOrCollinear` topLeft
            then (visited, activeViews) -- No compatible views. Return without altering visited or activeViews
            else let newVisited = Set.insert trueLocation visited
                     visionBlocked = vision trueLocation
                     in if visionBlocked
                        then let currentView = activeViews !! viewIndex -- Vision is blocked, calculate how it affects the view.
                                 shallowAboveBottomRight = (getShallowLine currentView) `isAbove` bottomRight
                                 steepBelowTopLeft = (getSteepLine currentView) `isBelow` topLeft
                                 in case (shallowAboveBottomRight, steepBelowTopLeft) of
                                    (True, True) -> (newVisited, remove viewIndex activeViews)
                                    (True, False) -> (newVisited, bumpAndCheck addShallowBump activeViews viewIndex topLeft)
                                    (False, True) -> (newVisited, bumpAndCheck addSteepBump activeViews viewIndex bottomRight)
                                    (False, False) -> let clonedViews = add viewIndex activeViews currentView in
                                                      let shallowChecked = bumpAndCheck addShallowBump clonedViews (viewIndex+1) topLeft in
                                                      let steepChecked = bumpAndCheck addSteepBump shallowChecked viewIndex bottomRight
                                                          in (newVisited, steepChecked)
                        else (newVisited,activeViews) -- Vision not blocked, we don't adjust any views.

-- | Calculates the index within the list that the view appropriate to the
--   Location specified has. If no view is appropriate to the Location
--   specified then the number returned is the length of the list.
calcViewIndex :: [View] -> Location -> Int
calcViewIndex activeViews bottomRight = go 0 activeViews bottomRight
    where go tmp views bottomRight = if tmp < (length views) && (getSteepLine (views!!tmp)) `isBelowOrCollinear` bottomRight
            then go (tmp+1) views bottomRight
            else tmp
