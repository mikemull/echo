module Echo.World where

import Data.Array
import Echo.Site

newtype SiteIndex = (Int, Int)

-- A world is a grid of sites
data World = World {
  sites :: Array SiteIndex Site 
  self_replication_threshold :: Float
  }


-- canSelfReplicate :: World -> Agent -> Bool
-- canSelfReplicate w a = True
