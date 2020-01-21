--module Main where

import MetaExtraction
import ProjectBuilder

import Text.Pandoc.Definition
import Text.Pandoc.JSON(toJSONFilter)
import Data.Maybe(mapMaybe)
import qualified Data.Map.Strict as Map

{-
FORMAT OF PROJECTS YAML DEFINITION
projects:
  -  fooProj:
      language: haskell
      root: /some/path
      srcRoots:
        - /path/1
        - /path/2
  -  barProj:
      language: scala
      root: /some/other/path
      # no extra src roots defined, use root as only src root
-}

main :: IO ()
main = toJSONFilter insertProjectBlocks


insertProjectBlocks:: Pandoc -> Pandoc
insertProjectBlocks (Pandoc meta blocks) = Pandoc metaWithoutProjects (metaBlocks ++ blocks)
  where
    metaWithoutProjects = Meta $ Map.delete "projects" $ unMeta meta
    extractionRes = extractProjectsMeta meta
    metaBlocks = mapMaybe toBlock $  extractionRes
    toBlock ::ExtractionResult ProjectVals -> Maybe Block
    toBlock (Left _) = Nothing --implement improved user feedback in case of erronous project definitions
    toBlock (Right p) = fmap renderProject $ projectFromVals p
