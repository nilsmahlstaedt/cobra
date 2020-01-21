module MetaExtraction (
  extractProjectsMeta,
  ProjectVals,
  ExtractionError,
  ExtractionResult,
  takeSuccess
) where

import Text.Pandoc.Definition
import Data.Maybe
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map

{-
FORMAT OF PROJECTS YAML DEFINITION
projects:
  fooProj:
      language: haskell
      root: /some/path
      srcRoots:
        - /path/1
        - /path/2
  barProj:
      language: scala
      root: /some/other/path
      # no extra src roots defined, use root as only src root
-}

{-
general note about the structure of this module,
once we find a matching yaml key "projects" we
require the contents of this block to adhere to the
structure as described above!

should this no be the case, the program in turn the whole filter and
conversion will stop with an error message!
-}

type ProjectVals = (Maybe String, Maybe String, Maybe String, Maybe [String])


--- new type structure to give adequate results for failing steps
data ExtractionError = MissingKey (Map.Map String MetaValue) String
                     | WrongType String String
                     | MissingValues String
                     --deriving (Show)

instance Show ExtractionError where
  show (MissingKey map key) = "could not find key '"++key++"' in map: " ++ show map
  show (WrongType expected actual)  = "Expected "++expected++" but got "++actual
  show  (MissingValues field) = "Missing value for field: "++field

type ExtractionResult t = Either ExtractionError t

--- return success value or throw contained error
takeSuccess:: ExtractionResult t -> t
takeSuccess (Left err) = error $ show err
takeSuccess (Right v) = v


extractProjectsMeta:: Meta -> [ExtractionResult ProjectVals]
extractProjectsMeta m = values
  where
    projects = Either.fromRight Map.empty (mvToMap' =<< lookupMeta' "projects" m)
    defTpl = Map.assocs projects
    values = map (uncurry projectValues) defTpl

projectValues:: String -> MetaValue -> ExtractionResult ProjectVals
projectValues key (MetaMap m) = Right (Just key, Just lang, Just root, Just srcs)
  where
    lang = takeSuccess $ mvToString' $ lookup' "language" m
    root = takeSuccess $ mvToString' $ lookup' "root" m
    srcs = takeSuccess $ fmap (mapMaybe mvToString) $ mvToList' $ fromMaybe (MetaList []) $ Map.lookup "srcRoots" m
projectValues _ mv = Left $ WrongType "MetaMap" (show mv)

mvToString :: MetaValue -> Maybe String
mvToString v =
  case v of
    MetaString s -> Just s
    MetaInlines [Str s] -> Just s
    MetaInlines xs -> Just (concat $ mapMaybe inlineToString xs)
    x -> Nothing

inlineToString :: Inline -> Maybe String
inlineToString i = case i of
  Str s -> Just s
  x -> Nothing



mvToList :: MetaValue -> Maybe [MetaValue]
mvToList v = case v of
  MetaList xs -> Just xs
  MetaMap xs -> Just [MetaMap xs]
  x -> error ("mvToListError: "++(show x))--Nothing

mvToMap :: MetaValue -> Maybe (Map.Map String MetaValue)
mvToMap v = case v of
  MetaMap m -> Just m
  x -> error ("mvToMap called on something not a map: "++(show x))--Nothing



-- helper functions for converting old functions
toExtResFunc':: (MetaValue -> Maybe a) -> (MetaValue -> ExtractionError) -> MetaValue -> ExtractionResult a
toExtResFunc' f errF v = toExtResFunc f (errF v) v

toExtResFunc:: (MetaValue -> Maybe a) -> ExtractionError -> MetaValue -> ExtractionResult a
toExtResFunc f err v = case f v of
  Just res -> Right res
  Nothing -> Left err

-- updated extraction functions with error returns
mvToMap':: MetaValue -> ExtractionResult (Map.Map String MetaValue)
mvToMap' = toExtResFunc' mvToMap (WrongType "MetaMap" . show)

lookupMeta':: String -> Meta -> ExtractionResult MetaValue
lookupMeta' key meta = case lookupMeta key meta of
  Just v -> Right v
  Nothing -> Left $ MissingKey (unMeta meta) key

mvToList' :: MetaValue -> ExtractionResult [MetaValue]
mvToList' = toExtResFunc' mvToList (WrongType "MetaList []" . show)

mvToString':: MetaValue -> ExtractionResult String
mvToString' = toExtResFunc' mvToString (WrongType "MetaString or MetaInlines [Inline.Str]" . show)

--- guaranteed map lookup with error message
lookup':: String -> Map.Map String MetaValue -> MetaValue
lookup' k m = fromMaybe (error $ show (MissingKey m k)) (Map.lookup k m)
