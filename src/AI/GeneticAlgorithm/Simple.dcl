definition module AI.GeneticAlgorithm.Simple

from AI.GeneticAlgorithm.Environment import :: Environment, :: JSONNode
from Data.Either import :: Either
from AI.GeneticAlgorithm.RandomUtil import :: RandomInts

:: Objective = Maximize Real | Minimize Real | Undefined
:: Weight :== Real

instance toString Objective

// | Chromosome interface
class Chromosome problem chromosome | == chromosome where
  // | Crossover function
  crossover :: RandomInts problem chromosome chromosome -> ([chromosome], RandomInts)
  // | Mutation function
  mutation :: RandomInts problem chromosome -> (chromosome, RandomInts)
  // | Fitness function. fitness x > fitness y means that x is better than y
  // | Lefts are always worse than Rigts. Lefts are meant to represent fitness for invalid
  // | chromosomes.
  fitness :: problem Environment chromosome -> Either [Objective] [Objective]
  // | The weights for the objectives. Use repeat 1.0 if not used. Chromosome is an unused
  // | parameter. It's there because Clean requires it.
  weights :: problem chromosome -> [Weight]


// Calculates the weighted product ratio. See
// https://en.wikipedia.org/wiki/Weighted_product_model
//
// Left is better than right if the weighted product is > 1
// Left is worse than right if the weighted product is < 1
//
// The two objective lists must have the same length, and the same objectives at the same indices.
// The list of weights also must have the same length
weightedProduct :: [Objective] [Objective] [Weight] -> Real


runGA ::
  RandomInts        // ^ Random number generator
  Int               // ^ Population size
  Real              // ^ Mutation probability [0, 1]
  (RandomInts -> (a, RandomInts)) // ^ Random chromosome generator (hint: use currying or closures)
  (a Int -> Bool)   // ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
  b                 // ^ Problem instance
  Environment       // ^ Environment
  -> [a]            // ^ Best chromosome
  | Chromosome b a
