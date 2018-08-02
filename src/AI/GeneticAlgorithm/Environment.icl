implementation module AI.GeneticAlgorithm.Environment

import StdBool
import StdFile
import StdMisc
import System.FilePath
import System.IO
import Text.GenJSON

readEnvironment :: FilePath *World -> ((Environment, Bool), *World)
readEnvironment fileName world
  # (ok, file, world) = fopen fileName FReadText world
  | not ok = ((JSONNull, False), world)
  // I don't know what this magic number means. It comes from System.IO
  # (str, file)       = freads file 16777216
  # (ok, world)       = fclose file world
  = ((fromString str, True), world)
