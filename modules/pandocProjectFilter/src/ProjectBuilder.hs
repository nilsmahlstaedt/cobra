{-# LANGUAGE OverloadedStrings #-}

module ProjectBuilder (
  Project,
  projectFromVals,
  renderProject
) where

import Text.Pandoc.Definition
import Formatting
import MetaExtraction(ProjectVals)
import Data.Maybe(fromMaybe)
import Data.List(intercalate)

data Project = Project {
 key :: String,
 language :: String,
 root :: String,
 srcRoots :: [String]
} deriving Show

data HTMLBlock = HTMLBlock {
  classes :: [String],
  dataAttributes :: [(String, String)]
} deriving Show

projectFromVals:: ProjectVals -> Maybe Project
projectFromVals (Just key, Just lang, Just root, srcRoot) = Just $ Project key lang root (fromMaybe [] srcRoot)
projectFromVals a = error ("called projectFromVals with missing required values (Input: "++show a++")")

renderProject:: Project -> Block
renderProject p = RawBlock (Format "html") (renderHTML $ projectToHTML p)

projectToHTML:: Project -> HTMLBlock
projectToHTML Project {key = k, language = l, root = r, srcRoots = rs} = HTMLBlock ["project-def"] dataAttrs
  where
    dataAttrs = [("key", k), ("language", l), ("root", r), ("srcRoots", mkString rs ",")]

renderHTML:: HTMLBlock -> String
renderHTML HTMLBlock {classes = cs, dataAttributes = das} = html
  where
    dataTags = map (uncurry $ formatToString ("data-" % string % "=\"" % string % "\"")) das
    renderedTags = mkString dataTags " "
    html = formatToString ("<div class=\"project-definition\" " % string % " ></div>") renderedTags

mkString:: [String] -> String -> String
mkString xs separator = intercalate separator xs