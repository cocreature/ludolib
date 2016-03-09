{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Criterion.Main

import Control.DeepSeq

import Data.Location
import Data.Set
import qualified Util.PPFOV as Curr
--import qualified Util.PPFOVNext as Next

-- | Orphan, but that's alright.
instance NFData Location where
    -- rnf :: a -> ()
    rnf l@(Location (x,y)) = x `seq` y `seq` l `seq` ()

range = 500

openVision = (\(Location (x,y)) -> x > 50 || y > 50)

-- Generated randomly with
-- replicateM 100 $ (,) <$> randomRIO (-50,50) <*> randomRIO (-50,50)
-- And then I bumped up any location with both x and y less than 10 by a bit.
crowdedSet :: Set Location
crowdedSet = fromList $ Location <$> [
    (-16,-24),(-14,26),(-14,26),(18,26),(41,-5),(11,-11),(-45,-1),(-15,2),(20,-26),(-46,-1),
    (49,-41),(-22,-36),(-16,-11),(27,-9),(18,11),(-37,-44),(-21,-35),(34,-3),(-5,-37),(-39,16),
    (37,-35),(16,-28),(-31,37),(-40,-1),(28,-31),(23,35),(-21,-43),(2,-12),(23,40),(-10,22),
    (-9,-19),(0,-29),(-32,10),(4,-26),(-21,-3),(-14,-25),(-49,-43),(16,-46),(-38,-49),(1,39),
    (-15,-29),(-31,41),(16,45),(-37,45),(13,-26),(-16,42),(33,44),(-8,8),(12,42),(-46,11),
    (-18,-26),(-45,49),(10,-46),(-46,-43),(-13,44),(-25,24),(36,19),(14,44),(-11,30),(-39,0),
    (13,-8),(33,18),(-6,-22),(-34,-20),(-39,28),(-38,6),(44,-9),(39,23),(16,-20),(44,-5),
    (-48,-38),(-8,-40),(18,-17),(50,-45),(8,16),(-39,50),(15,49),(-46,39),(-45,-6),(-29,16),
    (2,-42),(-33,-12),(-17,-16),(34,21),(-40,-45),(16,33),(-14,36),(-10,25),(35,-13),(-46,-29),
    (44,7),(-17,35),(28,-36),(-7,-27),(-20,-17),(-49,11),(9,33),(17,18),(-31,2),(-39,44)
    ]

crowdedVision = (\loc@(Location (x,y)) ->  x > 50 || y > 50 || (loc `elem` crowdedSet))

main = defaultMain [
    {-
    bench "Curr, Open, Range 10" $ nf (Curr.computeFOV openVision 10) (Location (0,0)),
    bench "Next, Open, Range 10" $ nf (Next.computeFOV openVision 10) (Location (0,0)),
    bench "Curr, Crowded, Range 10" $ nf (Curr.computeFOV crowdedVision 10) (Location (0,0)),
    bench "Next, Crowded, Range 10" $ nf (Next.computeFOV crowdedVision 10) (Location (0,0)),
    bench "Curr, Open, Range 20" $ nf (Curr.computeFOV openVision 20) (Location (0,0)),
    bench "Next, Open, Range 20" $ nf (Next.computeFOV openVision 20) (Location (0,0)),
    bench "Curr, Crowded, Range 20" $ nf (Curr.computeFOV crowdedVision 20) (Location (0,0)),
    bench "Next, Crowded, Range 20" $ nf (Next.computeFOV crowdedVision 20) (Location (0,0)),
    -}
    bench "Curr, Open, Range 50" $ nf (Curr.computeFOV openVision 50) (Location (0,0)),
    --bench "Next, Open, Range 50" $ nf (Next.computeFOV openVision 50) (Location (0,0)),
    bench "Curr, Crowded, Range 50" $ nf (Curr.computeFOV crowdedVision 50) (Location (0,0)),
    --bench "Next, Crowded, Range 50" $ nf (Next.computeFOV crowdedVision 50) (Location (0,0))
    ]
