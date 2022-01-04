import Data.Char

--1)
nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:r)
    | h == reverse h =
      let
          aux :: String -> Int
          aux "" = 0
          aux (x:xs)
            | x `elem` "aeiou" = 1 + aux xs
            | otherwise = aux xs
      in
       aux h + nrVocale r
    | otherwise = nrVocale r

--2)
f :: Int -> [Int] -> [Int]
f _ [] = []
f nr (x:xs)
  | x `mod` 2 == 0 = x:nr:(f nr xs)
  | otherwise = x:(f nr xs)

--3)
divizori :: Int -> [Int]
divizori x = [y | y <- [1..(div x 2)], mod x y == 0] ++ [x]

--4)
listadiv :: [Int] -> [[Int]]
listadiv x = [divizori y | y <- x]

--5a) recursiv
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b (x:xs)
  | a <= x && x <= b = x : inIntervalRec a b xs
  | otherwise = inIntervalRec a b xs

--5b) comprehension
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b sir = [y | y <- sir, y >= a, y<=b]

--6a) recursiv
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs)
  | x > 0 = 1 + pozitiveRec xs
  | otherwise = pozitiveRec xs

--6b) comprehension; ne trebuie functie de agregare pt ca vrem
-- sa intoarcem un int, nu un [int]
pozitiveComp :: [Int] -> Int
pozitiveComp sir = length [x | x<-sir, x>0]

--7a) recursiv
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec sir =
  let   aux [] _ = []
        aux (x:xs) i
          | mod x 2 == 1 = i : aux xs (i+1)
          | otherwise = aux xs (i+1)
  in
    aux sir 0

--7b) comprehension
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp sir = [snd x| x <- zip sir [0..((length sir) -1)], mod (fst x) 2 == 1]

--8a) recursiv
multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (x:xs)
  | isDigit x = digitToInt x * multDigitsRec xs
  | otherwise = multDigitsRec xs

--8b) comprehension
multDigitsComp :: String -> Int
multDigitsComp sir = foldl (*) 1 [digitToInt x | x<- sir, isDigit x]
