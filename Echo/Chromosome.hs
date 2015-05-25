module Echo.Chromosome where

import System.Random
import Data.Array
import Echo.Types

data ChromosomeType = InteractionTag
                    | MatingTag
                    | OffenseString
                    | DefenseString
                    | CombatCondition
                    | TradeCondition
                    | MatingCondition
                    | TradingResource
                      deriving (Ord, Enum, Eq, Ix)

type Chromosome = Array ChromosomeType [Resource]

randomGene :: Int -> IO [Resource]
randomGene n = do
               g <- newStdGen
               return $ take n (randomRs (0, 3) g)

randomChromosome :: IO Chromosome
randomChromosome = do
               genes <- mapM randomGene (replicate 8 4)
               return $ listArray (InteractionTag, TradingResource) genes
