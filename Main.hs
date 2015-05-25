
import Data.Array
import Echo.Chromosome

main = do
  c <- randomChromosome
  print (c ! MatingTag)
  print (c ! TradingResource)

