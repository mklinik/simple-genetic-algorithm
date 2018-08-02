definition module AI.GeneticAlgorithm.Environment

from System.IO import :: IO
from System.FilePath import :: FilePath
from Text.GenJSON import :: JSONNode

:: Environment :== JSONNode

readEnvironment :: FilePath *World -> ((Environment, Bool), *World)
