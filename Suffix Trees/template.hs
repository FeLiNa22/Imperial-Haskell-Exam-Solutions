data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------
-- Part I

isPrefix :: String -> String -> Bool
isPrefix "" _ = True
isPrefix _ "" = False
isPrefix (c:cs) (c':cs')
  | c == c'   = isPrefix cs cs'
  | otherwise = False

removePrefix :: String -> String -> String
--Pre: s is a prefix of s' 
removePrefix "" s = s
removePrefix (c:cs) (c':cs')
  = removePrefix cs cs'

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s  = s : suffixes (tail s)

isSubstring :: String -> String -> Bool
isSubstring sub s = or $ map (isPrefix sub) (suffixes s)

findSubstrings :: String -> String -> [Int]
findSubstrings sub s = [n | (s,n) <- zip (map (isPrefix sub) (suffixes s)) [0..], s]

------------------------------------------------------
-- Part II
getIndices :: SuffixTree -> [Int]
getIndices (Leaf i) = [i]
getIndices (Node xs) = concatMap getIndices trees
  where
    (_, trees) = unzip xs

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' sub (Node z) = concatMap getIndices' z
  where
    getIndices' (a, t)
      | null s = getIndices t
      | null s' = findSubstrings' s t
      | otherwise = []
      where
        (p,s,s') = partition sub a
findSubstrings' _ _ = []

-- got to get em bonus marks eh
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition str@(c:cs) str'@(c':cs')
  | c == c'   = (c:p,s,s')
  | otherwise = ([],str,str')
  where
    (p,s,s') = partition cs cs'
partition str str' = ([],str,str')

------------------------------------------------------
-- Part III

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (sub,n) (Node z) 
  | z == (map insert' z) = Node ((sub, Leaf n) : z)
  | otherwise = Node (map insert' z)
  where
    insert' (a,t)
      | null p = (a,t)
      | p == a = (a, insert (s_p,n) t)
      | p /= a = (p, Node [(s_p,Leaf n),(a_p,t)])
      where
        (p,s_p,a_p) = partition sub a
        
------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node 
        [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
     ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

