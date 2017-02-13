module Main where

import Data.List
import Text.Printf

import Dota.Interactions.Cleave.Types
import Dota.Interactions.Cleave.Values

cleave :: (TrapezoidCleave, Maybe CircleCleave) -> (Float, Maybe Float, Maybe Float)
cleave (new, old) = (newArea, originalArea, ratio)
    where newArea = cleaveArea new
          originalArea = fmap cleaveArea old
          ratio = fmap (newArea /) originalArea

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
