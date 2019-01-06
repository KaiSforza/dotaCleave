module Dota.Interactions.Cleave.Values where

import Dota.Interactions.Cleave.Types

-- http://dota2.gamepedia.com/Battle_Fury
-- bfury
bfury :: (TrapezoidCleave, CircleCleave)
bfury = ( bfuryTrap'''
        , CircleCleave {radius=280})

-- 7.20
bfuryTrap''' :: TrapezoidCleave
bfuryTrap''' = standardCleave {end=360, distance=650}
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
magnus = ( magnusTrap''
         , CircleCleave {radius=240})

-- 7.20 (same as bfury)
magnusTrap'' :: TrapezoidCleave
magnusTrap'' = bfuryTrap'''
-- 7.16
magnusTrap' :: TrapezoidCleave
magnusTrap' = standardCleave {end=330, distance=625}
-- 7.07
magnusTrap :: TrapezoidCleave
magnusTrap = standardCleave {end=240, distance=460}

-- http://dota2.gamepedia.com/Sven
sven :: (TrapezoidCleave, CircleCleave)
sven = ( svenTrap'
       , CircleCleave {radius=300})

-- 7.20 sven (same as bfury)
svenTrap''' :: TrapezoidCleave
svenTrap''' = bfuryTrap'''
-- 7.14 sven
svenTrap'' :: TrapezoidCleave
svenTrap'' = standardCleave {end=330, distance=625}
-- 7.05 sven 
svenTrap' :: TrapezoidCleave
svenTrap' = standardCleave {end=300, distance=570}
-- 7.00 sven
svenTrap :: TrapezoidCleave
svenTrap = standardCleave {end=300, distance=550}

-- http://dota2.gamepedia.com/Tiny
tiny :: (TinyCleave, CircleCleave)
tiny = ( TinyCleave {width=200, distance'=400}
       , CircleCleave {radius=400})

