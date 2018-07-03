implementation module AI.GeneticAlgorithm.RandomUtil

import Math.Random
import StdReal
import StdInt

split :: RandomInts -> (RandomInts, RandomInts)
split [r:rs] = (rs, genRandInt r)

// TODO: this is a bit shitty
randomR :: (Int, Int) RandomInts -> (Int, RandomInts)
randomR (lo,hi) rands | hi < lo = randomR (hi,lo) rands
randomR (lo,hi) [rand:rands] = (result, rands)
where
  mag = hi - lo
  [r:_] = genRandReal rand
  result = toInt (r * (toReal mag) + (toReal lo))
