module Dota.Interactions.Cleave.Values where

import Dota.Interactions.Cleave.Types

-- http://dota2.gamepedia.com/Battle_Fury
-- 7.00-7.01 bfury: standardCleave {end=280, distance=520}
bfury :: (TrapezoidCleave, Maybe CircleCleave)
bfury = ( standardCleave {end=300, distance=570}
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

