definition module AI.GeneticAlgorithm.Simple

import AI.GeneticAlgorithm.RandomUtil

// | Chromosome interface
class Chromosome a where
  // | Crossover function
  crossover :: RandomInts a a -> ([a], RandomInts)
  // | Mutation function
  mutation :: RandomInts a -> (a, RandomInts)
  // | Fitness function. fitness x > fitness y means that x is better than y
  fitness :: a -> Real


runGA ::
  RandomInts        // ^ Random number generator
  Int               // ^ Population size
  Real              // ^ Mutation probability [0, 1]
  (RandomInts -> (a, RandomInts)) // ^ Random chromosome generator (hint: use currying or closures)
  (a Int -> Bool)   // ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
  -> a              // ^ Best chromosome
  | Chromosome a
