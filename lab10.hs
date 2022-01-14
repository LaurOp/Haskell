import Data.Maybe
data Expr = Const Int
          | Expr :+: Expr
          | Expr :*: Expr
           deriving Eq

data Operation = Add | Mult
                deriving (Eq, Show)

data Tree = Lf Int
          | Node Operation Tree Tree
           deriving (Eq, Show)

--1.1
instance Show Expr where
  show (Const x) = "Const x"
  show (x :+: y) = "(" ++ show x ++ " :+: " ++ show y ++ ")"
  show (x :*: y) = "(" ++ show x ++ " :*: " ++ show y ++ ")"

--1.2
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (x :+: y) = evalExp x + evalExp y
evalExp (x :*: y) = evalExp x * evalExp y

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 -- 6
test12 = evalExp exp2 -- 14
test13 = evalExp exp3 -- 13
test14 = evalExp exp4 -- 16

--1.3
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add x y) = evalArb x + evalArb y
evalArb (Node Mult x y) = evalArb x * evalArb y

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)
test21 = evalArb arb1 -- 6
test22 = evalArb arb2 -- 14
test23 = evalArb arb3 -- 13
test24 = evalArb arb4 -- 16

--1.4
expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (x :+: y) = Node Add (expToArb x) (expToArb y)
expToArb (x :*: y) = Node Mult (expToArb x) (expToArb y)

--2
class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key => key -> value -> c key value -> c key value
  lookup2 :: Ord key => Eq key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  toListBST :: c key value -> [(key,Maybe value)]
  fromList :: Ord key => [(key,value)] -> c key value
--2.1
  keys c = [fst x | x <- toList c]
  values c = [snd x | x <- toList c]
  fromList [] = empty
  fromList (x:xs) = insert (fst x) (snd x) (fromList xs)
--2.2
newtype PairList k v = PairList { getPairList :: [(k, v)] }
instance Collection PairList where
  empty = PairList []
  singleton k v = PairList [(k,v)]
  insert k v x = PairList ((getPairList x) ++ [(k,v)])
  lookup2 k x
    | length [snd y | y <- getPairList x, fst y == k] == 0 = Nothing
    | otherwise = [Just (snd y) | y <- getPairList x, fst y == k]!!0
  delete k x = PairList [y | y <- getPairList x, fst y /= k]
  toList x = getPairList x
  toListBST _ = undefined
--2.3
data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                   -- cheia elementului
      (Maybe value)         -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

--ordonez dupa cheie(pt ca s-a pus constraintul Ord pe key, nu pe value in Collection) desi e mai logic dupa valoare, ca altfel dau de erori
--la comparatia cu Maybe si ar trb sa adaug constrainturi in clasa Collecion
instance Collection SearchTree where
  empty = Empty
  singleton k v = BNode Empty k (Just v) Empty
  insert k v (Empty) = singleton k v
  insert k v (BNode stanga cheie val dreapta)
    | k < cheie = insert k v stanga
    | otherwise = insert k v dreapta
  lookup2 _ (Empty) = Nothing
  lookup2 k (BNode stanga cheie val dreapta)
    | k == cheie = val
    | k < cheie = lookup2 k stanga
    | otherwise = lookup2 k dreapta
  delete _ (Empty) = Empty
  delete k (BNode stanga cheie val dreapta)
    | k == cheie = (BNode stanga cheie Nothing dreapta)
    | k < cheie = (BNode (delete k stanga) cheie val dreapta)
    | otherwise = (BNode stanga cheie val (delete k dreapta))
  toList = undefined
  toListBST (BNode stanga cheie Nothing dreapta) = toListBST(stanga) ++ [(cheie, Nothing)] ++ toListBST(dreapta)
  toListBST (BNode stanga cheie (Just val) dreapta) = toListBST(stanga) ++ [(cheie, Just val)] ++ toListBST(dreapta)
  toListBST _ = []
