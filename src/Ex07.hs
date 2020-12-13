{-# LANGUAGE OverloadedStrings #-}

module Ex07 where
import Protolude
import qualified Data.Array as Array
import qualified Data.Text as Text
import qualified Data.Graph as Graph
import Data.Graph (Graph, Vertex)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Paths_adventofcode2020 ( getDataFileName )
import Util ( unsafeReadText, indexed )
import Data.List.Extra (nubOrd)

type IndexMap = HashMap Text Vertex
type LabelMap = HashMap Vertex Text
type WeightMap = HashMap (Vertex, Vertex) Int

parseBag :: HasCallStack => Text -> Maybe (Text, Int)
parseBag "no other bags." = Nothing
parseBag bagText = Just (adj <> " " <> color, unsafeReadText numT)
  where
    [numT, adj, color, _] = Text.splitOn " " bagText

parseRule :: HasCallStack => Text -> (Text, [(Text, Int)])
parseRule rule = (container, mapMaybe parseBag splitContents)
  where
    [container, contents] = Text.splitOn " bags contain " rule
    splitContents = Text.splitOn ", " contents

parseRules :: HasCallStack => Text -> [(Text, [(Text, Int)])]
parseRules = map parseRule . Text.splitOn "\n" . Text.strip

collectBagNames :: [(Text, [(Text, Int)])] -> [Text]
collectBagNames = nubOrd . concatMap go
  where
    go :: (Text, [(Text, Int)]) -> [Text]
    go (bag, bags) = bag : map fst bags

children :: Graph -> Vertex -> [Vertex]
children graph vertex = graph Array.! vertex
  

buildBagEdges :: 
  HasCallStack => 
  HashMap Text Int ->
 (Text, [(Text, Int)]) ->
 [(Graph.Edge, Int)]
buildBagEdges indexMap (container, contents) = 
  [(( indexMap HashMap.! fst bag
    , indexMap HashMap.! container ), snd bag) | bag <- contents]

buildBagGraph :: 
  [(Text, [(Text, Int)])] -> 
  (Graph, IndexMap, LabelMap, WeightMap)
buildBagGraph bagMap = (graph, indexMap, labelMap, weightMap)
  where
    indexMap = HashMap.fromList (map swap (HashMap.toList labelMap))
    labelMap = HashMap.fromList (indexed (collectBagNames bagMap))
    edges = concatMap (buildBagEdges indexMap) bagMap
    weightMap = HashMap.fromList edges
    graph = Graph.buildG (0, HashMap.size labelMap - 1) (map fst edges)

countContainers :: Text -> Graph -> IndexMap -> LabelMap -> Maybe Int
countContainers color bagGraph indexMap _labelMap = do
  colorIndex <- HashMap.lookup color indexMap
  let reach = Graph.reachable bagGraph colorIndex
  Just (length (nubOrd reach) - 1)

countContents :: HasCallStack => Graph -> WeightMap -> Vertex -> Int
countContents graph weightMap = go
 where 
  go vertex = 1 + sum (zipWith (*) weights (map go vertexChildren))
    where
      vertexChildren = children graph vertex
      weights = map (\c -> weightMap HashMap.! (c, vertex)) vertexChildren

invertGraph :: Graph -> Graph
invertGraph g = Graph.buildG (Array.bounds g) (map swap (graphToEdges g))

graphToEdges :: Graph -> [Graph.Edge]
graphToEdges graph = [(from_, to_) | (from_, is) <- Array.assocs graph, to_ <- is]

main :: IO ()
main = do
  filename <- getDataFileName "inputs/ex07test.txt"
  rules <- parseRules <$> readFile filename
  let 
    -- rulesWithoutCount = map (second (map fst)) rules
    (bagGraph, indexMap, labelMap, weightMap) = buildBagGraph rules
    Just shinyGold = HashMap.lookup "shiny gold" indexMap

  print (countContainers "shiny gold" bagGraph indexMap labelMap)
  print (countContents (invertGraph bagGraph) weightMap shinyGold - 1)