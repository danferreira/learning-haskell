{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield att def) = do
       attDice <- rollDice maxAtt
       defDice <- rollDice maxDef
       let result = map (uncurry (>)) $ zip (sortDice attDice) (sortDice defDice)
           attLost = length $ filter id result
           defLost = length $ filter not result
       return (Battlefield (att - attLost) (def - defLost))
       where
         maxAtt = if att >= 4 then 3 else att-1
         maxDef = if def >= 2 then 2 else def
         rollDice n = replicateM n die
         sortDice = reverse . sort

-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield att def)
              | (def <= 0) || (att < 2) = return bf
              | otherwise = battle bf >>= invade

-- Exercise 4

times = 1000

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM times (invade bf) >>= calculate

calculate :: [Battlefield] -> Rand StdGen Double
calculate bfs = return $ fromIntegral wins/fromIntegral times
      where
        wins = length $ filter ((== 0) . defenders) bfs
