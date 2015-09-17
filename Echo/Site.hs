module Echo.Site where

import Control.Monad.State.Lazy
import System.Random
import Data.Maybe
import Echo.Agent
import Echo.Chromosome

-- A site is basically a ring (circular list) of agents, plus resources
data Site = Site {
  agents :: [Agent],
  res_prod :: [Int]
} deriving (Show)


testSite = Site $ map (testAgent.("t" ++).show) [1..10]

randomSite :: Int -> IO Site
randomSite n = do
               agents <- mapM randomAgent $ map (("a" ++).show) [1..n]
               return $ Site agents [5,5,5,5]

addChild p c = case c of
                       Nothing -> [p]
                       Just c -> [subRes p c , c]

repAgents :: Site -> IO Site
repAgents s = do
        newGen <- mapM rep (agents s)
        -- go through the maybes and subtract resources from parents
        return $ s {agents=(foldl (++) [] (zipWith addChild (agents s) newGen))}


resCounts s = foldl (zipWith (+)) [0,0,0,0] $ map resForRep (agents s)


moveAgent :: Site -> Site
moveAgent s = s {agents=(tail x) ++ [(head x)]}
              where x = (agents s)

-- An interaction is combat or (trade and/or mating) between two random agents
interact :: Site -> IO (Int, Int)
interact s = do
             g <- newStdGen
             let (a1,g2) = randomR (0, length (agents s)) g
             let a2 = head $ dropWhile (==a1) $ randomRs (0, length (agents s)) g2
             return (a1,a2)
