implementation module AI.GeneticAlgorithm.Simple

import StdInt
import Data.List
import StdEnum
import qualified Data.Foldable as F
import Data.Func
import StdTuple
import StdOrdList
import StdFunc
import Data.Maybe

import StdMisc

import AI.GeneticAlgorithm.RandomUtil

runGA ::
  RandomInts        // ^ Random number generator
  Int               // ^ Population size
  Real              // ^ Mutation probability [0, 1]
  (RandomInts -> (a, RandomInts)) // ^ Random chromosome generator (hint: use currying or closures)
  (a Int -> Bool)   // ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
  -> a              // ^ Best chromosome
  | Chromosome a
runGA gen ps mp rnd stopf =
  let (pop, gen_) = zeroGeneration gen rnd ps in
  runGA_ gen_ pop ps mp stopf 0

runGA_ :: RandomInts [a] Int Real (a Int -> Bool) Int -> a | Chromosome a
runGA_ gen pop ps mp stopf gnum =
  let best = head pop in
  if (stopf best gnum)
    (best)
    (let (pop_, gen_) = nextGeneration gen pop ps mp in
     runGA_ gen_ pop_ ps mp stopf (gnum+1)
    )

// | Generate zero generation. Use this function only if you are going to implement your own runGA.
zeroGeneration ::
  RandomInts          // ^ Random number generator
  (RandomInts -> (a, RandomInts)) // ^ Random chromosome generator (hint: use closures)
  Int                 // ^ Population size
  -> ([a],RandomInts) // ^ Zero generation and new RNG
zeroGeneration initGen rnd ps =
  foldl
    (\(xs, gen) _ -> let (c, gen_) = rnd gen in ([c:xs], gen_))
    ([], initGen)
    [1..ps]

nextGeneration ::
  RandomInts  // ^ Random number generator
  ![a]         // ^ Current generation
  Int         // ^ Population size
  Real        // ^ Mutation probability
  -> ([a], RandomInts) // ^ Next generation ordered by fitness (best - first) and new RNG
  | Chromosome a
nextGeneration gen pop ps mp =
  let
    (gen_, gens) = case unfoldr (Just o split) gen of
      [g:gs] -> (g, gs)
      []     -> abort "empty gens"
    chunks = zip2 gens $ init $ tails pop
    results =
      map
        (\x -> case x of
            (g, [x:ys]) ->
                [ (t, fitness t)
                \\ t <- nextGeneration_ [ (x, y) \\ y <- ys ] g mp []
                ]
            (_, []) -> abort "empty chunk")
        chunks
    lst = take ps $ sortBy (\(_, fx) (_, fy) -> fy < fx) $ 'F'.concat results
  in ( map fst lst, gen_ )

nextGeneration_ [] _ _ acc = acc
nextGeneration_ [(p1,p2):ps] g0 mp acc =
  let
    (children0, g1) = crossover g0 p1 p2
    (children1, g2) =
      foldl
        (\(xs, g) x -> let (x_, g_) = mutate g x mp in ([x_:xs], g_))
        ([],g1) children0
  in
  nextGeneration_ ps g2 mp (children1 ++ acc)

mutate :: RandomInts a Real -> (a, RandomInts) | Chromosome a
mutate [rand:rands] x mp =
  let [r:_] = genRandReal rand in
  if (r <= mp)
    (mutation rands x)
    (x, rands)
