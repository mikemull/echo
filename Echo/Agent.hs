module Echo.Agent where

import Control.Applicative ((<$>))
import Data.Array
import Data.List
import Echo.Types
import Echo.Chromosome

data Agent = Agent {
  name :: String,
  chromosome :: Chromosome,
  reservoir :: [Int]
  } deriving(Show)

testAgent name = Agent name testChromosome [10,10,10,10]


randomAgent :: String -> IO Agent
randomAgent name = do
                   c <- randomChromosome
                   return $ Agent name c [10,10,10,10]

countR r rs = length $ filter (==r) rs

resForRep a1 = (sequence $ countR <$> [A,B,C,D]) $ concat.elems $ (chromosome a1)

insufficientResource p_agent = find (<0) $ zipWith (-) (reservoir p_agent) (map (*selfRepThreshold) (resForRep p_agent))

reservoirShare x = map (\x-> quot x 2) x

rep :: Agent -> Maybe Agent
rep p_agent = case insufficientResource p_agent of
                      Just _ -> Nothing
                      Nothing -> Just $  Agent "x1" (chromosome p_agent) (reservoirShare (reservoir p_agent))

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


