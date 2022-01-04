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
  lookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value
  --keys col _ _ = [x | <- ]
