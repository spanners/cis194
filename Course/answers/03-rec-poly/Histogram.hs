{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}

module Histogram where

import qualified Data.Map as D

histogram :: [Integer] -> [String]
histogram = map tower . freq

freq :: forall k a. (Ord k, Num a) => [k] -> [(k, a)]
freq input = D.toList $ D.fromListWith (+) [(c, 1) | c <- input]

tower :: (a, Integer) -> String
tower (_, frq) = concat ["*\n" | _ <- [0..frq]]

main :: IO ()
main = undefined
