import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame = check . nub
 where
  check :: [a] -> Bool
  check []  = True
  check [_] = True
  check _   = False


remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x = filter ((/= x) . fst)

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt attname header row = lookUp attname (zip (map fst header) row)

removeAtt :: AttName -> Header -> Row -> Row
removeAtt attname header row = map snd (remove attname (zip (map fst header) row))

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (x, y) [] = [(x, [y])]
addToMapping (x, y) xyss@(xy@(x', y') : xys)
 | x == x'   = (x, y : y') : xys
 | otherwise = xy : addToMapping (x, y) xys

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (attName, attValues) (header, rows) = [(v, length (filter ( == v) rowValues)) | v <- attValues] 
 where attIndex  = fromJust (findIndex (( == attName) . fst) header )
       rowValues = map (!!attIndex) rows
--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes (Leaf _)    = 1
nodes (Node _ ts) = 1 + sum (map (nodes . snd) ts)
nodes _           = 0

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null     header row      = ""
evalTree (Leaf v) header row      = v
evalTree (Node att ts) header row = evalTree tree header row
 where 
  attValue = lookUpAtt att header row
  tree     = lookUp attValue ts
--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------
--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (header, rows) att@(attName, attValues) =
   [(v, (headerMod, map (removeAtt attName header) (filter (( == v) . (lookUpAtt attName header)) rows))) | v <- attValues]
 where headerMod = header \\ [att]

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree (_, [])          _        _        = Null
buildTree (header, rows) classifier@(attName, attValues) selector 
 | allSame (map (lookUpAtt attName header) rows) = Leaf (lookUpAtt attName header (rows!!0))
 | otherwise                                     = Node  name [(v, buildTree ds (attName, attValues) selector) | (v, ds) <- p]
 where
  nextAtt@(name, values) = selector (header, rows) classifier
  p       = partitionData (header, rows) nextAtt
--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------



entropy :: DataSet -> Attribute -> Double
entropy ds@(header, rows) attName@(att, attValues)
 | rows == [] = 0.0
 | rows /= [] = res 
 where freq        = buildFrequencyTable attName (header, rows)
       n           = (fromIntegral . length) rows
       res         = - sum [ let num = fromIntegral (lookUp v freq) in xlogx (num / n) | v <- attValues]
gain :: DataSet -> Attribute -> Attribute -> Double
gain ds@(header, rows) p@(attName, attValues) c@(attName', attValues') = res
  where 
    e    = entropy ds c
    lmao = partitionData ds p
    freq = buildFrequencyTable p ds
    n    = (fromIntegral . length) rows
    res  =  e - sum [let p = (fromIntegral (lookUp v freq)) / n in (p * (entropy (lookUp v lmao) c)) | v <- attValues]     
bestGainAtt :: AttSelector
bestGainAtt ds@(header, rows) c@(attName, attValues) = lookUp maxGain (zip gains atts)
 where atts  = filter (/= c) header
       gains = map (flip (gain (header, rows)) c)  atts
       maxGain = maximum gains
--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]