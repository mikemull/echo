module Echo.Agent where

import System.Random.MWC
import Control.Applicative ((<$>))
import Control.Monad.State
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


randomAgent :: GenIO -> String -> IO Agent
randomAgent g name = do
                   c <- randomChromosome g
                   return $ Agent name c [10,10,10,10]

countR r rs = length $ filter (==r) rs

resForRep a1 = (sequence $ countR <$> [A,B,C,D]) $ concat.elems $ (chromosome a1)

insufficientResource p_agent = find (<0) $ zipWith (-) (reservoir p_agent) (map (*selfRepThreshold) (resForRep p_agent))

reservoirShare x = map (\x-> quot x 2) x

subRes a1 a2 = a1 {reservoir=(zipWith (-) (reservoir a1) (reservoir a2))} 

addRes :: GenIO -> [Float] -> Agent -> IO Agent
addRes g prob a = do
           rinc <- resInc g prob
           return $ a {reservoir=(zipWith (+) (reservoir a) rinc)}

resInc g prob = replicateM (length prob) (uniform g) >>= \x -> return $ zipWith (\r s -> if r>s then 1 else 0) x prob  


rep :: GenIO -> Agent -> IO (Maybe Agent)
rep g p_agent = case insufficientResource p_agent of
                      Just _ -> return Nothing
                      Nothing -> do
                            childChrom <- mutateChromosome g (chromosome p_agent)
                            return $ Just $  Agent ("c" ++ name p_agent) childChrom (reservoirShare (reservoir p_agent))

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

