module Dota.Interactions.Cleave.Types where

data TrapezoidCleave = TrapezoidCleave { start :: Float
                                       , end :: Float
                                       , distance :: Float }
                                       deriving (Show, Eq)

newtype CircleCleave = CircleCleave { radius :: Float }
    deriving (Show, Eq)

-- New cleave area is described using the cleave radius, as shown below:
--      End Radius
--     ├───┤
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
    cleaveVolume x = Right $ (distance x * pi * (start x ^ 2 + (end x * start x) + end x ^ 2)) / 3
    cleaveAngle x = Right $ atan ((end x - start x) / distance x)
    cleaveLength x = sqrt $ end x ^ 2 + distance x ^ 2

instance Cleave CircleCleave where
    cleaveArea x = (radius x ^ 2) * pi
    cleaveVolume _ = Left "Pretty sure it's just a cylinder."
    cleaveAngle _ = Left "It's a circle."
    cleaveLength x = radius x * 2

-- Cleave generally starts at 150, so just make an easy default that actually
-- doesn't cleave at all
standardCleave :: TrapezoidCleave
standardCleave = TrapezoidCleave {start = 150, end = 150, distance = 0}
