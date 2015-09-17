module Echo.Chromosome where

import System.Random
import Data.Array
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


mutateChromosome :: Chromosome -> IO Chromosome
mutateChromosome c = do
               genes <- mapM randomMutate (elems c)
               return $ listArray (InteractionTag, TradingResource) genes

randomMutate r = (randomIO :: IO Float) >>= \x -> if (x < mutationProb) then mutateGene r else return r

mutateGene :: [Resource] -> IO [Resource]
mutateGene r = do
         g <- newStdGen
         let (n,g2) = randomR (0, length r) g
         mutate r n
         where mutate r n
                    | n == length r =  insertion r
                    | n == (length r)-1 = deletion r
                    | otherwise = pointMutate r n          

insertion r = (randomGene 1) >>= \x -> return (r ++ x)

pointMutate r n = (randomGene 1) >>= \x -> return $ let (a,b) = (splitAt n r) in (a ++ x) ++ (tail b)

deletion r = do
       g <- newStdGen
       let (n,g2) = randomR (0, fromEnum (maxBound :: Resource) + 1) g
       if n == fromEnum (maxBound :: Resource)
          then return (init r)
          else return r
