definition module AI.GeneticAlgorithm.Environment

from System.IO import :: IO
from System.FilePath import :: FilePath
from Text.GenJSON import :: JSONNode

// use jsonQuery to read items in the Environment
:: Environment :== JSONNode

readEnvironment :: FilePath *World -> ((Environment, Bool), *World)

emptyEnvironment :: Environment
