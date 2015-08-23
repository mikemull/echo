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
                      deriving (Ord, Enum, Eq, Ix, Show)

type Chromosome = Array ChromosomeType [Resource]

testChromosome :: Chromosome
testChromosome = listArray (InteractionTag, TradingResource) [[x] | x <- (replicate 8 A)]

randomGene :: Int -> IO [Resource]
randomGene n = do
               g <- newStdGen
               return $ take n (randoms g :: [Resource])

-- Start with genes of length 1
-- The TradingResource gene can only have length 1, should it be handled specially?
randomChromosome :: IO Chromosome
randomChromosome = do
               genes <- mapM randomGene (replicate 8 1)
               return $ listArray (InteractionTag, TradingResource) genes
