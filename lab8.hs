import Data.Maybe
import Data.List
import Data.Char
type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving Eq
infixr 1 :->:
infixr 1 :<->:
infixr 2 :|:  --se executa inainte de :&:
infixr 3 :&:

--Ex1
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: ((Not (Var "P")) :&: (Not (Var "Q")))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: (((Not (Var "P")) :|: (Not (Var "Q"))) :&: ((Not (Var "P")) :|: (Not (Var "R"))))

--Ex2
instance Show Prop where
  show (Var x) = x
  show F = "False"
  show T = "True"
  show (Not x) = "(~" ++ show x ++ ")"
  show (x :|: y) = "(" ++ show x ++ "|" ++ show y ++ ")"
  show (x :&: y) = "(" ++ show x ++ "&" ++ show y ++ ")"
  show (x :->: y) = "(" ++ show x ++ "->" ++ show y ++ ")"
  show (x :<->: y) = "(" ++ show x ++ "<->" ++ show y ++ ")"

test_ShowProp :: Bool
test_ShowProp = show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

--Ex3
type Env = [(Nume, Bool)]
impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

(>>>) :: Bool -> Bool -> Bool
True >>> True = True
True >>> False = False
False >>> _ = True

(<=>) :: Bool -> Bool -> Bool
True <=> True = True
False <=> False = True
True <=> False = False
False <=> True = False

eval :: Prop -> Env -> Bool
eval (Var x) e = impureLookup x e
eval F _ = False
eval T _ = True
eval (Not x) e = not (eval x e)
eval (x :|: y) e = (eval x e) || (eval y e)
eval (x :&: y) e = (eval x e) && (eval y e)
eval (x :->: y) e = (eval x e) >>> (eval y e)
eval (x :<->: y) e = (eval x e) <=> (eval y e)

test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

--Ex4
variabile :: Prop -> [Nume]
variabile (Var x) = [x]
variabile F = []
variabile T = []
variabile (Not x) = variabile x
variabile (x :|: y) = nub (variabile x ++ variabile y)
variabile (x :&: y) = nub (variabile x ++ variabile y)
variabile (x :->: y) = nub (variabile x ++ variabile y)
variabile (x :<->: y) = nub (variabile x ++ variabile y)

test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]
test_variabile2 = variabile p3

--Ex5 sequence face produs cartezian pentru orice dimensiune
envs :: [Nume] -> [Env]
envs num = sequence [ [(x,y) |  y <- [False, True] ] | x <- num ]

test_envs = envs["P", "Q"] == [ [ ("P",False), ("Q",False)], [ ("P",False), ("Q",True)], [ ("P",True), ("Q",False)], [ ("P",True), ("Q",True)]]

--Ex6
satisfiabila :: Prop -> Bool
satisfiabila pr =  or [eval pr x | x <- (envs (variabile pr))]

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

--Ex7
valida :: Prop -> Bool
valida pr = and [eval pr x | x <- (envs (variabile pr))]

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

--Ex8 nu exista in pdf ??
--Ex9 le-am adaugat in definitie; evaluarea se face cu operatorii >>> si <=>
--Ex10
--egale e o auxiliara care verif ca doua liste sa fie egale
egale :: (Eq a) => [a] -> [a] -> Bool
egale [] [] = True
egale _ [] = False
egale [] _ = False
egale (x:xs) (y:ys)
  | x /= y = False
  | otherwise = egale xs ys

echivalenta :: Prop -> Prop -> Bool
echivalenta pr1 pr2 = egale rez1 rez2
  where
    var = nub ( (variabile pr1)++(variabile(pr2)) )
    rez1 = [eval pr1 x | x <- (envs var)]
    rez2 = [eval pr2 x | x <- (envs var)]

test_echivalenta1 = True == (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 = False == (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 = True == (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))
