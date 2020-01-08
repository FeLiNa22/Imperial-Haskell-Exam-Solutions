import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT (TFun t t') 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv 
  = TypeTable    -- i.e. [(String, Type)]

type Sub 
  = TypeTable    -- i.e. [(String, Type)]  

-- Built-in function types...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

------------------------------------------------------
-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a lst = tryToLookUp a undefined lst

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp a d lst = maybe d id (lookup a lst)

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp b lst = [a' | (a',b')<-lst, b' == b]

occurs :: String -> Type -> Bool
occurs str (TVar str') = str == str'
occurs str (TFun t t') = occurs str t || occurs str t'
occurs str _ = False

------------------------------------------------------
-- PART II

-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All type variables in the expression have a binding in the given 
--      type environment
inferType :: Expr -> TEnv -> Type
inferType (Number _) _   = TInt
inferType (Boolean _) _  = TBool
inferType (Prim p) _     = lookUp p primTypes
inferType (Id str) env   = lookUp str env
inferType (Cond e a a') env
  | inferType e env == TBool && 
    (inferType a env == inferType a' env) 
              = inferType a env
  | otherwise = TErr
inferType app@(App _ _) env = inferApp app env

inferApp :: Expr -> TEnv -> Type
inferApp (App f a) env = inferApp' (inferType f env) (inferType a env) 
  where
    inferApp' (TFun d b) a
      | a == d    = b
      | otherwise = TErr
  
------------------------------------------------------
-- PART III

applySub :: Sub -> Type -> Type

applySub s (TFun t t') = TFun (applySub s t) (applySub s t')
applySub s t@(TVar v)  = tryToLookUp v t s
applySub _ t           = t

unify :: Type -> Type -> Maybe Sub
unify t t' = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] s = Just s
unifyPairs ((t,t'):xs) s = unifyPairs' t t'
  where
    unifyPairs' TInt  TInt = unifyPairs xs s
    unifyPairs' TBool TBool = unifyPairs xs s
    unifyPairs' (TVar v) t@(TVar v')
      | v == v' = unifyPairs xs s
      | otherwise =  call v t xs s 
    unifyPairs' (TVar v) t
      =  call v t xs s 
    unifyPairs' t (TVar v)
      = call v t xs s 
    unifyPairs' (TFun t1 t2) (TFun t1' t2') 
      = unifyPairs ((t1,t1') : (t2,t2') : xs) s
    unifyPairs' _ _ = Nothing
    call v t xs s 
      | occurs v t = Nothing
      | otherwise = unifyPairs 
          [(applySub [(v,t)] x, applySub [(v,t)] y) | (x,y)<-xs]
            ((v,t):s)

------------------------------------------------------
-- PART IV

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv env tsub
  = map modify env
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs = undefined

inferPolyType :: Expr -> Type
inferPolyType e = t
  where
    (_,t,_) = inferPolyType' e [] 0
   
-- You may optionally wish to use one of the following helper function declarations
-- as suggested in the specification. 

inferPolyType' :: Expr -> TEnv -> Int -> (Sub, Type, Int)
inferPolyType' (Number _) _ n = ([],TInt,n)
inferPolyType' (Boolean _) _ n = ([],TBool,n)
inferPolyType' (Prim p) _ n = ([],lookUp p primTypes,n)
inferPolyType' (Id str) env n = ([],lookUp str env,n)
inferPolyType' (App e e') _ n = ([],undefined,n)
inferPolyType' (Fun x e) env n 
  | te == TErr = (s,TErr,n') 
  | otherwise  = (s,(TFun (applySub s (TVar ('$' : show n))) te),n')
    where 
      (s,te,n') = inferPolyType' e ((x,TVar ('$':show n)):env) (n+1)

------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))
