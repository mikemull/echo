import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Vector (singleton)
import Echo.Site
import System.Random.MWC


main = do
  g <- initialize (singleton 42)
  n <- getLine
  s <- randomSite g 50
  let sim = replicateM (read n) (step g)
  execStateT sim s >>= stats


step :: GenIO -> StateT Site IO ()
step g = do
  -- For each site:
  --  interactions
  --  tax
  --  kill
  --  migrate
  s <- get
  --  distribute resources
  s' <- lift $ distRes g s
  --  replicate
  s'' <- lift $ repAgents g s'
  put s''


stats :: Site -> IO ()
stats s = do
  putStrLn $ show $ length (agents s)
