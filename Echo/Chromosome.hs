module Echo.Chromosome where

import System.Random.MWC
import Data.Array
import Data.Vector (Vector, toList)
import Echo.Types

data GeneType = InteractionTag
              | MatingTag
              | OffenseString
              | DefenseString
              | CombatCondition
              | TradeCondition
              | MatingCondition
              | TradingResource
              deriving (Ord, Enum, Eq, Bounded, Ix, Show)

type Chromosome = Array GeneType [Resource]

testChromosome :: Chromosome
testChromosome = listArray (InteractionTag, TradingResource) [[x] | x <- (replicate 8 A)]

randomGene :: GenIO -> Int -> IO [Resource]
randomGene g n = do
               v <- uniformVector g n :: IO (Vector Resource)
               return $ toList v


-- Start with genes of length 1
-- The TradingResource gene can only have length 1, should it be handled specially?
randomChromosome :: GenIO -> IO Chromosome
randomChromosome g = do
               genes <- mapM (randomGene g) (replicate 8 1)
               return $ listArray (InteractionTag, TradingResource) genes


mutateChromosome :: GenIO -> Chromosome -> IO Chromosome
mutateChromosome g c = do
               genes <- mapM (randomMutate g) (elems c)
               return $ listArray (InteractionTag, TradingResource) genes

randomMutate g r = (uniform g :: IO Float) >>= \x -> if (x < mutationProb) then mutateGene g r else return r

mutateGene :: GenIO -> [Resource] -> IO [Resource]
mutateGene g r = do
         n <- uniformR (0, length r) g
         mutate r n g
         where mutate r n g
                    | n == length r =  insertion r g
                    | n == (length r)-1 = deletion r g
                    | otherwise = pointMutate r n g         

insertion r g = (randomGene g 1) >>= \x -> return (r ++ x)

pointMutate r n g = (randomGene g 1) >>= \x -> return $ let (a,b) = (splitAt n r) in (a ++ x) ++ (tail b)

deletion r g = do
       n <- uniformR (0, fromEnum (maxBound :: Resource) + 1) g
       if n == fromEnum (maxBound :: Resource)
          then return (init r)
          else return r
