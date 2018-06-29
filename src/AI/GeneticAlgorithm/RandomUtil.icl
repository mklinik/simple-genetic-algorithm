implementation module AI.GeneticAlgorithm.RandomUtil

import Math.Random

split :: RandomInts -> (RandomInts, RandomInts)
split [r:rs] = (rs, genRandInt r)
