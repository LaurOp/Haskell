--1
firstEl :: [(a,b)] -> [a]
firstEl x = (map fst x)

--2
sumList :: [[Int]] -> [Int]
sumList x = map sum x

--3
prel2 :: [Int] -> [Int]
prel2 x = let aux y
                | y `mod` 2 == 0 = y `div` 2
                | otherwise = y*2
          in map aux x

--4
checkChar :: Char -> [String] -> [String]
checkChar x y = filter (elem x) y

--5
patrateImpare :: [Int] -> [Int]
patrateImpare x = map (^2) (filter odd x)

--6
patratePozImpare :: [Int] -> [Int]
patratePozImpare x = let oddaux y
                          | (snd y) `mod` 2 == 1 = True
                          | otherwise = False
                    in map (^2) (map fst (filter oddaux (zip x [0..(length x) - 1])))

--7
numaiVocale :: [String] -> [String]
numaiVocale x = let isVowel c = elem c "aeiouAEIOU"
                    vowels s = filter isVowel s
                in map (vowels) x

--8
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap op (x:xs) = (op x) : mymap op xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter op (x:xs)
  | op x = x : myfilter op xs
  | otherwise = myfilter op xs

--9
sumPatrImp :: [Int] -> Int
sumPatrImp x = foldr (+) 0 (patrateImpare x)

--10
checkList :: [Bool] -> Bool
checkList [] = False
checkList x = foldr (&&) True x

--11a)
rmChar :: Char -> String -> String
rmChar x y = let aux z = x /= z
             in filter (aux) y

--11b)
rmCharsRec :: String -> String -> String
rmCharsRec "" a = a
rmCharsRec a "" = ""
rmCharsRec (x:xs) y = rmCharsRec xs (rmChar x y)

--11c)
rmCharsFold :: String -> String -> String
rmCharsFold x y = foldr (\z -> if not (z `elem` x) then (z:) else id) "" y
