import Data.List
import Data.Char

--1
factori :: Int -> [Int]
factori x = [n | n <- [1..x `div` 2] , x `mod` n == 0] ++ [x]

--2
prim :: Int -> Bool
prim n = length (factori n) == 2

--3
numerePrime :: Int -> [Int]
numerePrime x = [n | n <- [2..x], prim n]

--4
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
myzip3 _ _ [] = []
myzip3 _ [] _ = []
myzip3 [] _ _ = []
myzip3 (a:as) (b:bs) (c:cs)
  | as/=[] && bs/=[] && cs/=[] = (a,b,c) : myzip3 as bs cs
  | otherwise = [(a,b,c)]

--5
ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
--ordonataNat x = and [x!!a <= x!!(a+1) | a <- [0..(length x - 2)] ] fara ZIP
ordonataNat (x:xs) = and [a <= b | (a, b) <- zip (x:xs) xs]

--6
ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (x:xs)
  | x <= head xs = ordonataNat1 xs
  | otherwise = False

--7
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] _ = True
ordonata [x] _ = True
ordonata (x:xs) semn
  | x `semn` (head xs) = ordonata xs semn
  | otherwise = False

--8 Voi simula operatorul < pe ambele elemente, exemplu (1,2) *<* (2,3) dar (1,2) nu e *<* (2,2)
(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(*<*) (a,b) (c,d)
  | a < c && b < d = True
  | otherwise = False

--9 ruleaza, am verif
compuneList :: (b -> c) -> [(a -> b)] -> [(a -> c)]
compuneList fct [] = []
compuneList fct (x:xs) = fct.x : compuneList fct xs

--10
aplicaList :: a -> [(a -> b)] -> [b]
aplicaList val [] = []
aplicaList val (x:xs) = x val : aplicaList val xs
