module Echo.Site where

import Echo.Agent

-- A site is basically a ring (circular list) of agents, plus resources
data Site = Site {
  agents :: [Agent]
}


