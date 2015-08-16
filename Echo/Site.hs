module Echo.Site where

import Control.Monad.State.Lazy
import System.Random
import Echo.Agent

-- A site is basically a ring (circular list) of agents, plus resources
data Site = Site {
  agents :: [Agent]
}


testSite = Site $ map (testAgent.("t" ++).show) [1..10]

randomSite :: Int -> IO Site
randomSite n = do
               agents <- mapM randomAgent $ map (("a" ++).show) [1..n]
               return $ Site agents

moveAgent :: Site -> Site
moveAgent s = Site $ (tail x) ++ [(head x)]
              where x = (agents s)

-- An interaction is combat or (trade and/or mating) between two random agents
interact :: Site -> IO (Int, Int)
interact s = do
             g <- newStdGen
             let a1 = head $ randomRs (0, length (agents s)) g
             let a2 = head $ dropWhile (==a1) $ randomRs (0, length (agents s)) g
             return (a1,a2)

