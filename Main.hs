import Control.Monad
import Control.Monad.State
import Data.Array
import Echo.Site

main = do
  s <- randomSite 10
  -- execStateT replicateM 3 $ step s
  let sim = replicateM 3 step
  execStateT sim s >>= print


step :: StateT Site IO ()
step = do
  -- For each site:
  --  interactions
  --  collect resources
  --  tax
  --  kill
  --  produce resources
  --  migrate
  --  replicate
  s <- get
  s' <- lift $ repAgents s
  put s'
