module Echo.Site where

import System.Random
import Echo.Agent

-- A site is basically a ring (circular list) of agents, plus resources
data Site = Site {
  agents :: [Agent]
}


testSite = Site $ map (testAgent.("t" ++).show) [1..10]

moveAgent :: Site -> Site
moveAgent s = Site $ (tail x) ++ [(head x)]
              where x = (agents s)

interact :: Site -> IO (Int, Int)
interact s = do
             g <- newStdGen
             let a1 = head $ take 1 $ randomRs (0, length (agents s)) g
             let a2 = head $ take 1 $ dropWhile (==a1) $ randomRs (0, length (agents s)) g
             return (a1,a2)

