definition module AI.GeneticAlgorithm.Simple

from AI.GeneticAlgorithm.Environment import :: Environment, :: JSONNode
from Data.Either import :: Either
from AI.GeneticAlgorithm.RandomUtil import :: RandomInts

// | Chromosome interface
class Chromosome problem chromosome where
  // | Crossover function
  crossover :: RandomInts problem chromosome chromosome -> ([chromosome], RandomInts)
  // | Mutation function
  mutation :: RandomInts problem chromosome -> (chromosome, RandomInts)
  // | Fitness function. fitness x > fitness y means that x is better than y
  // | Lefts are always worse than Rigts. Lefts are meant to represent fitness for invalid
  // | chromosomes.
  fitness :: problem Environment chromosome -> Either Real Real


runGA ::
  RandomInts        // ^ Random number generator
  Int               // ^ Population size
  Real              // ^ Mutation probability [0, 1]
  (RandomInts -> (a, RandomInts)) // ^ Random chromosome generator (hint: use currying or closures)
  (a Int -> Bool)   // ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
  b                 // ^ Problem instance
  Environment       // ^ Environment
  -> a              // ^ Best chromosome
  | Chromosome b a
