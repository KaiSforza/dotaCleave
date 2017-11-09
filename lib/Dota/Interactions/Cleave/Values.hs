module Dota.Interactions.Cleave.Values where

import Dota.Interactions.Cleave.Types

-- http://dota2.gamepedia.com/Battle_Fury
-- 7.07 bfury
bfury :: (TrapezoidCleave, CircleCleave)
bfury = ( bfuryTrap''
        , CircleCleave {radius=280})

-- 7.07
bfuryTrap'' :: TrapezoidCleave
bfuryTrap'' = standardCleave {end=330, distance=625}
-- 7.02 bfury
bfuryTrap' :: TrapezoidCleave
bfuryTrap' = standardCleave {end=300, distance=570}

-- 7.00-7.01 bfury: standardCleave {end=280, distance=520}
bfuryTrap :: TrapezoidCleave
bfuryTrap = standardCleave {end=280, distance=520}

-- http://dota2.gamepedia.com/Kunkka
kunkka :: (TrapezoidCleave, CircleCleave)
-- 7.00 -7.01 cleave: standardCleave {end=600, distance=900}
kunkka = ( standardCleave {end=650, distance=1100}
         , CircleCleave {radius=600})

-- http://dota2.gamepedia.com/Magnus
magnus :: (TrapezoidCleave, CircleCleave)
magnus = ( standardCleave {end=240, distance=460}
         , CircleCleave {radius=240})

-- http://dota2.gamepedia.com/Sven
sven :: (TrapezoidCleave, CircleCleave)
sven = ( standardCleave {end=300, distance=550}
       , CircleCleave {radius=300})

-- http://dota2.gamepedia.com/Tiny
tiny :: (TrapezoidCleave, CircleCleave)
tiny = ( standardCleave {end=400, distance=600}
       , CircleCleave {radius=400})

