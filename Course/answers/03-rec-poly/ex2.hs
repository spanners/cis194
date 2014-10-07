{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnicodeSyntax #-}

import Data.List (tails)

pattern Three x y z ← x:y:z:_

localMaxima :: [Integer] → [Integer]
localMaxima xs = [ mid | Three left mid right ← tails xs, left < mid, mid > right ]
