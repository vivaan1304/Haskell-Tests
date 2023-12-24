type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node k _ _) = k

rank :: BinTree a -> Int
rank (Node _ r _) = r

children :: BinTree a -> [BinTree a]
children (Node _ _ xs) = xs

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t1 t2
 | value t1 > value t2  = Node (value t2) (rank t1 + 1) (t1 : children t2)
 | value t1 <= value t2 = Node (value t1) (rank t1 + 1) (t2 : children t1)

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin (t : []) = value t
extractMin (t : ts)
  | extractMin ts < (value t)  = extractMin ts
  | extractMin ts >= (value t) = value t

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps t1s_@(t1 : t1s) t2s_@(t2 : t2s)
 | rank t1 < rank t2  = t1 : mergeHeaps t1s t2s_
 | rank t1 > rank t2  = t2 : mergeHeaps t1s_ t2s 
 | rank t1 == rank t2 = mergeHeaps [(combineTrees t1 t2)] (mergeHeaps t1s t2s) 
mergeHeaps [] t2s_ = t2s_
mergeHeaps t1s_ [] = t1s_

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert x ts = mergeHeaps [Node x 0 []] ts

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin ts = mergeHeaps (reverse (children delTree)) rest
 where
  (delTree, rest) = lookUp (extractMin ts) [] ts
  -- pre: m exists in the heap
  -- finds the first BinTree of the heap which has root value = m
  lookUp :: Eq a => a ->  BinHeap a -> BinHeap a -> (BinTree a, BinHeap a)
  lookUp m cur (t : ts)
   | (value t) == m = (t, cur ++ ts)
   | otherwise      = lookUp m (cur ++ [t]) ts
   
remove :: Eq a => a -> BinHeap a -> BinHeap a
remove
  = undefined

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin
  = undefined

binSort :: Ord a => [a] -> [a]
binSort xs = binSort' xs []
 where
  binSort' :: Ord a => [a] -> BinHeap a -> [a]
  binSort' []       []   = []
  binSort' (x : xs) heap = binSort' xs (insert x heap)
  binSort' []       heap = (extractMin heap) : binSort' [] (deleteMin heap) 

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary ts = toBinary' ts (-1)
 where
  toBinary' :: BinHeap a -> Int -> [Int]
  toBinary' [] i       = []
  toBinary' (t : ts) i = (toBinary' ts r) ++ (1 : (take (r - i - 1) (repeat 0)))
   where r = rank t


binarySum :: [Int] -> [Int] -> [Int]
binarySum as bs = binarySum' (reverse as) (reverse bs) 0
 where
  getSum :: Int -> Int -> Int -> (Int,Int)
  getSum a b c
   | sum == 0 || sum == 1 = (0, sum)
   | sum == 2             = (1, 0)
   | sum == 3             = (1, 1)
   where sum = a + b + c
  
  binarySum' :: [Int] -> [Int] -> Int -> [Int]
  binarySum' []       []       carry
   | carry == 0 = []
   | carry == 1 = [1]
  binarySum' (a : as) []       carry =  let (carry', sum) = getSum a 0 carry in binarySum' as [] carry'  ++ [sum]
  binarySum' []       (b : bs) carry =  let (carry', sum) = getSum 0 b carry in binarySum' bs [] carry'  ++ [sum]
  binarySum' (a : as) (b : bs) carry =  let (carry', sum) = getSum a b carry in binarySum' as bs carry'  ++ [sum]


------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]



