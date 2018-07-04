implementation module AI.GeneticAlgorithm.RandomUtil

import Math.Random
import StdReal
import StdInt

split :: RandomInts -> (RandomInts, RandomInts)
split [r:rs] = (rs, genRandInt r)

randomR :: (Int, Int) RandomInts -> (Int, RandomInts)
randomR (lo,hi) rands | hi < lo = randomR (hi,lo) rands
randomR (lo,hi) [rand:rands] = (result, rands)
where
  mag = hi - lo
  r = rand rem mag
  r_ = if (r < 0) (r + mag) r
  result = r + lo
