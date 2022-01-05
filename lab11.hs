import Data.Maybe
import Data.Char
import Data.List

type Name = String
data Value = VBool Bool
  | VInt Int
  | VFun (Value -> Value)
  | VError

--Eu de unde sa ghicesc de la ce vin prescurtarile hlit hvar hlam :$: ?
data Hask = HTrue | HFalse
  | HIf Hask Hask Hask
  | HLit Int
  | Hask :==: Hask
  | Hask :+: Hask
  | HVar Name
  | HLam Name Hask
  | Hask :$: Hask
infix 4 :==:
infixl 6 :+:
infixl 9 :$:

type HEnv = [(Name, Value)]
--1
instance Show Value where
  show (VBool True) = "True"
  show (VBool False) = "False"
  show (VInt x) = show x
  show (VFun _) = "Functie"
  show (VError) = "Eroare"

--2
instance Eq Value where
  (VBool x) == (VBool y) = x == y
  (VInt x) == (VInt y) = x == y
  (VBool x) == (VInt y) = (x==False && y==0) || (x==True && y==1)
  (VInt y) == (VBool x) = (x==False && y==0) || (x==True && y==1)
  (VFun f) == _ = error "eq pe functie"
  (VError) == _ = error "eq pe eroare"

--3 n-a prea iesit
(+++) :: Value -> Value -> Int
(VInt x) +++ (VInt y) = x + y
hEval :: Hask -> HEnv -> Value
hEval HTrue _ = VBool True
hEval HFalse _ = VBool False
hEval (HIf c d e) r = hif (hEval c r) (hEval d r) (hEval e r)
  where hif (VBool b) v w = if b then v else w
        hif _ _ _ = VError
hEval (HLit x) r = VInt x
hEval (x :==: y) r = VBool (hEval x r == hEval y r)
--hEval (x :+: y) r = (hEval x r + hEval y r)
hEval (HVar n) r = [snd x | x<-r , fst x == n]!!0
hEval (HLam n f) r = undefined
hEval (x :$: y) r = undefined

--4 degeaba il fac daca nu merge 3
run :: Hask -> String
run pg = show (hEval pg [])
