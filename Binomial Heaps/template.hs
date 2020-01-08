type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

key :: BinTree a -> a
key (Node k _ _) = k

rank :: BinTree a -> Int
rank (Node _ r _) = r

children :: BinTree a -> [BinTree a]
children (Node _ _ c) = c

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t1@(Node k1 r1 c1) t2@(Node k2 r2 c2)
  | k1 < k2 = Node k1 (r1+1) (t2:c1)
  | otherwise = Node k2 (r2+1) (t1:c2)

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin heap@(x:xs) = extractMin' heap (key x)
  where
    extractMin' [] min = min
    extractMin' (x:xs) min
      | key x < min = extractMin' xs (key x)
      | otherwise =  extractMin' xs min  

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] h2 = h2
mergeHeaps h1 [] = h1
mergeHeaps h1 h2
  | rank t < rank t' = t : mergeHeaps (tail h1) h2
  | rank t > rank t' = t' : mergeHeaps h1 (tail h2)
  | otherwise         = mergeHeaps [combineTrees t t'] (mergeHeaps (tail h1) (tail h2))
  where
    t  = head h1 
    t' = head h2


insert :: Ord a => a -> BinHeap a -> BinHeap a
insert val h = mergeHeaps [Node val 0 []] h

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h 
  = mergeHeaps (reverse $ children minTree) xsHeap
    where
      (minTree, xsHeap) = removeMin h

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove _ [] = []
remove a (x:xs) 
  | key x == a  = xs
  | otherwise   = x : remove a xs

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin h = (minTree h, remove minVal h)
  where
    minTree (x:xs)
      | key x  == minVal = x
      | otherwise        = minTree xs
    minVal = extractMin h

binSort :: Ord a => [a] -> [a]
binSort lst = extractOrder (buildHeap lst [])
  where
    buildHeap [] h = h
    buildHeap (x:xs) h = buildHeap xs (insert x h)

    extractOrder [] = []
    extractOrder h = extractMin h : extractOrder (deleteMin h)
--------------------------------------------------------------
-- PART III

-- toBinary :: BinHeap a -> [Int]
toBinary h = toBinary' h 0 []
  where
    toBinary' [] _ acc = acc
    toBinary' h@(x:xs) index acc
      | rank x > index = toBinary' h (index+1) (0:acc)
      | otherwise      = toBinary' xs (index+1) (1:acc)

binarySum :: [Int] -> [Int] -> [Int]
binarySum b1 b2
  | length b1 < length b2 = binarySum (replicate (length b2 - length b1) 0 ++ b1) b2
  | length b1 > length b2 = binarySum b1 (replicate (length b1 - length b2) 0 ++ b2)
  | otherwise = take (length b1) sum
  where
    binSum (0,0) (_,0) = (0,0)
    binSum (1,0) (_,0) = (1,0) 
    binSum (0,1) (_,0) = (1,0) 
    binSum (1,1) (_,0) = (0,1) 
    binSum (0,0) (_,1) = (1,0) 
    binSum (1,0) (_,1) = (0,1) 
    binSum (0,1) (_,1) = (0,1) 
    binSum (1,1) (_,1) = (1,1) 
    (sum,carry) = unzip (scanr (binSum) (0,0) (zip b1 b2))

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



