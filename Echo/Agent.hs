module Echo.Agent where

import Data.Array
import Data.List
import Echo.Types
import Echo.Chromosome

data Agent = Agent {
  name :: String,
  chromosome :: Chromosome,
  reservoir :: [Resource],
  makeup :: [Resource]
  }

testAgent name = Agent name testChromosome [10,10] [10, 10]

willAttack :: Agent -> Agent -> Bool
willAttack a1 a2 = isPrefixOf ((chromosome a1) ! CombatCondition)  ((chromosome a2) ! InteractionTag) -- Interaction or Offense??

combatPayoff :: Agent -> Agent -> Int
combatPayoff a1 a2 = hamming ((chromosome a1) ! OffenseString)  ((chromosome a2) ! DefenseString)
         
hamming :: (Eq a) => [a] -> [a] -> Int
hamming [] ys = length ys
hamming xs [] = length xs 
hamming (x:xs) (y:ys)
 | x /= y = 1 + hamming xs ys
 | otherwise = hamming xs ys


