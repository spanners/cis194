{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           =   first DV 
                     . randomR (1,6)
  randomR (low,hi) =   first DV 
                     . randomR ( max 1 (unDV low)
                               , min 6 (unDV hi) )

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army
                               , defenders :: Army } 
                            deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
             as <- dice attacking
             ds <- dice defending
             return $ battle' b as ds
           where attacking = min 3 (attackers b)
                 defending = min 2 (defenders b) 

battle' :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
battle' b aDice dDice = 
  uncurry Battlefield $
  foldl (\(a,d) (aDie,dDie) ->
      if aDie > dDie
          then (a,d - 1)
          else (a - 1, d))
      (attackers b, defenders b)
      (zip (sortBy (flip compare) aDice)
           (sortBy (flip compare) dDice))
 
example :: Battlefield
example = Battlefield 93 96

result :: Battlefield
result = evalRand (battle example) (mkStdGen 4)

invade :: Battlefield -> Rand StdGen Battlefield
invade b = undefined

isEndGame :: Battlefield -> Bool
isEndGame b = defenders b == 0 || attackers b < 2
