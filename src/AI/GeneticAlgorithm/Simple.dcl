definition module AI.GeneticAlgorithm.Simple

import AI.GeneticAlgorithm.RandomUtil

// | Chromosome interface
class Chromosome problem a where
  // | Crossover function
  crossover :: RandomInts problem a a -> ([a], RandomInts)
  // | Mutation function
  mutation :: RandomInts problem a -> (a, RandomInts)
  // | Fitness function. fitness x > fitness y means that x is better than y
  fitness :: problem a -> Real


runGA ::
  RandomInts        // ^ Random number generator
  Int               // ^ Population size
  Real              // ^ Mutation probability [0, 1]
  (RandomInts -> (a, RandomInts)) // ^ Random chromosome generator (hint: use currying or closures)
  (a Int -> Bool)   // ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
  b                 // ^ Problem instance
  -> a              // ^ Best chromosome
  | Chromosome b a
