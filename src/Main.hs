module Main where

import Data.List
import Text.Printf
import Control.Applicative

data TrapezoidCleave = TrapezoidCleave { start :: Float
                                       , end :: Float
                                       , distance :: Float }
                                       deriving (Show, Eq)

data CircleCleave = CircleCleave { radius :: Float }
    deriving (Show, Eq)

-- New cleave area is described using the cleave radius, as shown below:
--      End Radius
--     ┝━━━┥
-- ╲───┬───╱ ┬
--  ╲  │  ╱  │ Distance
--   ╲─┴─╱   ┴
--     ├─┤
--      Start Radius
--
-- The formula for getting the new radius would therefore be
--      ((start radius * 2 + end radius * 2) / 2) * distance
-- which simplifies to
--      (start radius + end radius) * distance
-- 
-- The old cleave was simply a circle tangential to the target, so it just
-- uses
--      pi * radius²
class Cleave a where
    cleaveArea :: a -> Float
    cleaveVolume :: a -> Either String Float
    -- Angle of one side relative to a line perpendicular to the radii
    cleaveAngle :: a -> Either String Float
    -- Longest distance you can cleave from
    cleaveLength :: a -> Float

instance Cleave TrapezoidCleave where
    cleaveArea x = (start x + end x) * distance x
    cleaveVolume x = Right $ (distance x * pi * ((start x) ^ 2 + (end x * start x) + (end x) ^ 2)) / 3
    cleaveAngle x = Right $ atan ((end x - start x) / distance x)
    cleaveLength x = sqrt $ (end x - start x) ^ 2 + (distance x) ^ 2

instance Cleave CircleCleave where
    cleaveArea x = ((radius x) ^ 2) * pi
    cleaveVolume _ = Left "Pretty sure it's just a cylinder."
    cleaveAngle _ = Left "It's a circle."
    cleaveLength x = (radius x) * 2

-- Cleave generally starts at 150, so just make an easy default that actually
-- doesn't cleave at all
standardCleave :: TrapezoidCleave
standardCleave = TrapezoidCleave {start = 150, end = 150, distance = 0}

-- http://dota2.gamepedia.com/Battle_Fury
bfury :: (TrapezoidCleave, Maybe CircleCleave)
bfury = ( standardCleave {end=280, distance=520}
        , Just (CircleCleave {radius=280}))

-- http://dota2.gamepedia.com/Kunkka
kunkka :: (TrapezoidCleave, Maybe CircleCleave)
kunkka = ( standardCleave {end=600, distance=900}
         , Just (CircleCleave {radius=600}))

-- http://dota2.gamepedia.com/Magnus
magnus :: (TrapezoidCleave, Maybe CircleCleave)
magnus = ( standardCleave {end=240, distance=460}
         , Just (CircleCleave {radius=240}))

-- http://dota2.gamepedia.com/Sven
sven :: (TrapezoidCleave, Maybe CircleCleave)
sven = ( standardCleave {end=300, distance=550}
       , Just (CircleCleave {radius=300}))

-- http://dota2.gamepedia.com/Tiny
tiny :: (TrapezoidCleave, Maybe CircleCleave)
tiny = ( standardCleave {end=400, distance=600}
       , Just (CircleCleave {radius=400}))

cleave :: (TrapezoidCleave, Maybe CircleCleave) -> (Float, Maybe Float, Maybe Float)
cleave (new, old) = (newArea, originalArea, ratio)
    where newArea = cleaveArea new
          originalArea = fmap cleaveArea old
          ratio = fmap ((/) newArea) originalArea

-- These numbers are not rounded, just truncated.
formatReddit :: (PrintfArg t, Floating t) => String -> (t, Maybe t, Maybe t) -> String
formatReddit name (newArea, Just originalArea, Just ratio) =
    printf "* %s: `%.f` (~%.1f%%) (original area: ~`%.f`)" name newArea (ratio * 100) originalArea

-- In case we don't have an original cleave (new items)
formatReddit name (newArea, _, _) = printf "* %s: `%.f`" name newArea

main :: IO ()
main = putStr $ unlines
    [ formatReddit "Battlefury" $ cleave bfury
    , formatReddit "Empower" $ cleave magnus
    , formatReddit "Grow with Aghs" $ cleave tiny
    , formatReddit "Tidebringer" $ cleave kunkka
    , formatReddit "Great Cleave" $ cleave sven
    ]

-- Output:
-- * Battlefury: `223600` (~90.8%) (original area: ~`246301`)
-- * Empower: `179400` (~99.1%) (original area: ~`180956`)
-- * Grow with Aghs: `330000` (~65.7%) (original area: ~`502655`)
-- * Tidebringer: `675000` (~59.7%) (original area: ~`1130973`)
-- * Great Cleave: `247500` (~87.5%) (original area: ~`282743`)
