module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp e lst
  = head [b | (a,b)<-lst , e == a]

-- 3 marks
vars :: Formula -> [Id]
vars f = sort $ nub $ (vars' f )
    where
      vars' (Var x) = [x]
      vars' (And x y)  = vars' x ++ vars' y 
      vars' (Or x y) = vars' x ++ vars' y 
      vars' (Not x) = vars' x 
      
-- 1 mark
idMap :: Formula -> IdMap
idMap f = zip (vars f) [1..] 

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF

toNNF (Not (Not(x)))  = toNNF x
toNNF (Not (And x y)) = Or (toNNF (Not x)) (toNNF (Not y))
toNNF (Not (Or x y))  = And (toNNF (Not x)) (toNNF (Not y))
toNNF (And x y)       = And (toNNF x) (toNNF y)
toNNF (Or x y)        = Or (toNNF x) (toNNF y)
toNNF (Not x)         = Not (toNNF x)
toNNF (Var x)         = Var x

-- 3 marks
toCNF :: Formula -> CNF
toCNF f = toCNF' (toNNF f)
  where 
    toCNF' (Or x y)   = distribute (toCNF' x) (toCNF' y)
    toCNF' (And x y)  = And (toCNF' x) (toCNF' y)
    toCNF' (Not x)    = Not (toCNF' x)
    toCNF' (Var x)    = Var x

-- 4 marks
flatten :: CNF -> CNFRep
flatten cnf =  (flatten' cnf )
    where 
      flatten' (And x y) = flatten' x ++ flatten' y
      flatten' (Or x y) = [(head $ flatten' x) ++ (head $ flatten' y)]
      flatten' (Var e) = [[val e]]
      flatten' (Not (Var e)) = [[negate $ val e]]
      val e = lookUp e (idMap cnf)
--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits cnfrep = recurse' [] cnfrep
    where 
      recurse' units props
        | single == Nothing = (props,units)
        | otherwise = applyRules single
          where 
            single = getSingleton props
            getSingleton [] = Nothing
            getSingleton (x:xs)
              | isSingle x = Just (head x)
              | otherwise = getSingleton xs
              where 
                isSingle [a] = True
                isSingle _     = False
            
            applyRules (Just x) = recurse' (x:units) rem2
              where
                rem1 = map (delete (-x)) props
                rem2 = filter (not . elem x) rem1

-- 4 marks
dp :: CNFRep -> [[Int]]
dp cnfrep =  filter ([] /= ) $ map nub [dp' n, dp' (-n)] 
  where
    n = head $ head initProp
    dp' n = initUnits ++ recurse n initProp []
    (initProp,initUnits) = propUnits cnfrep

    recurse num [] units = units
    recurse num propped units
      | elem [] nextProp = []
      | otherwise        = recurse n nextProp (units ++ nextUnits)
        where
          (nextProp,nextUnits) = propUnits ([num] : propped)
          n = head $ head propped

      
--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined


