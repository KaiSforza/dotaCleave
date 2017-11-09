module Main where

import Data.List
import Text.Printf

import Dota.Interactions.Cleave.Types
import Dota.Interactions.Cleave.Values

cleave :: (Cleave t, Cleave c) => t -> c -> (Float, Float, Float)
cleave new old = (newArea, originalArea, ratio)
    where newArea = cleaveArea new
          originalArea = cleaveArea old
          ratio = newArea / originalArea

-- These numbers are not rounded, just truncated.
formatReddit :: (PrintfArg t, Floating t) => String -> (t, t, t) -> String
formatReddit name (newArea, originalArea, ratio) =
    printf "* %s: `%.f` (~%.1f%%) (original area: ~`%.f`)" name newArea (ratio * 100) originalArea

-- -- In case we don't have an original cleave (new items)
-- formatReddit name (newArea, _, _) = printf "* %s: `%.f`" name newArea

main :: IO ()
main = putStr $ unlines
    [ formatReddit "Battlefury" $ uncurry cleave bfury
    , formatReddit "Bfury 7.06->7.07" $ cleave bfuryTrap bfuryTrap'
    , formatReddit "Bfury 7.01->7.02" $ cleave bfuryTrap' bfuryTrap''
    , formatReddit "Empower" $ uncurry cleave magnus
    , formatReddit "Grow with Aghs" $ uncurry cleave tiny
    , formatReddit "Tidebringer" $ uncurry cleave kunkka
    , formatReddit "Great Cleave" $ uncurry cleave sven
    ]

-- Output:
-- * Battlefury: `300000` (~121.8%) (original area: ~`246301`)
-- * Bfury 7.06->7.07: `300000` (~117.0%) (original area: ~`256500`)
-- * Bfury 7.01->7.02: `256500` (~114.7%) (original area: ~`223600`)
-- * Empower: `179400` (~99.1%) (original area: ~`180956`)
-- * Grow with Aghs: `330000` (~65.7%) (original area: ~`502655`)
-- * Tidebringer: `880000` (~77.8%) (original area: ~`1130973`)
-- * Great Cleave: `247500` (~87.5%) (original area: ~`282743`)
