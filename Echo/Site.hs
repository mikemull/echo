module Echo.Site where

import Control.Monad.State.Lazy
import System.Random.MWC
import Data.Maybe
import Echo.Agent
import Echo.Chromosome

-- A site is basically a ring (circular list) of agents, plus resources
data Site = Site {
  agents :: [Agent],
  res_prod :: [Int]
} deriving (Show)


testSite = Site $ map (testAgent.("t" ++).show) [1..10]

randomSite :: GenIO -> Int -> IO Site
randomSite g n = do
               agents <- mapM (randomAgent g) $ map (("a" ++).show) [1..n]
               return $ Site agents [5,5,5,5]

addChild p c = case c of
                       Nothing -> [p]
                       Just c -> [subRes p c , c]

repAgents :: GenIO -> Site -> IO Site
repAgents g s = do
        newGen <- mapM (rep g) (agents s)
        -- go through the maybes and subtract resources from parents
        return $ s {agents=(foldl (++) [] (zipWith addChild (agents s) newGen))}


resCounts s = foldl (zipWith (+)) [0,0,0,0] $ map resForRep (agents s)

resProb s = map (\(x,y) -> min 1 (fromIntegral y/(fromIntegral x))) $ zip (resCounts s) (res_prod s)

distRes g s = do
      newAgents <- mapM (addRes g (resProb s)) (agents s)
      return $ s {agents=newAgents}

moveAgent :: Site -> Site
moveAgent s = s {agents=(tail x) ++ [(head x)]}
              where x = (agents s)

otherAgent g a a1 = do
             a2 <- uniformR (0, length a) g
             if a2 /= a1
             then
                return a2
             else
                otherAgent g a a1


-- An interaction is combat or (trade and/or mating) between two random agents
interact :: GenIO -> Site -> IO (Int, Int)
interact g s = do
             a1 <- uniformR (0, length (agents s)) g
             a2 <- otherAgent g (agents s) a1
             return (a1,a2)
