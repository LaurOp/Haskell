--1
import Control.Exception
import Data.Char
rotate :: Int -> [Char] -> [Char]
rotate n sir =  let poz = zip sir [0..length sir]
                in
                  if n < 0 || n > length sir then error "crapa de la numar"
                  else [fst x | x<-poz, snd x >= n] ++ [fst x | x<-poz, snd x < n];

--2;
-- prop_rotate e si o functie din Pictures.hs care face rotate pe o imagine 2D gen stanga-dreapta

-- functia nu verifica nimic interesant, intoarce True mereu pt ca roteste cu l-m pozitii sirul rotit cu m pozitii,
-- deci roteste cu l pozitii, unde l este lungimea lui str , deci str ramane str mereu si intra doar pe ramura True

-- evita aruncarea erorii verificand daca lungimea str e 0, ca sa nu faca mod din 0 , si
-- prin reducerea lui k astfel incat sa nu poata depasi lungimea str
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
  where l = length str
        m = if l == 0
              then 0
            else k `mod` l

--3
makeKey :: Int -> [(Char,Char)]
makeKey val = let aux = ['A'..'Z']
              in zip aux (rotate val aux)

--4
lookUp :: Char -> [(Char,Char)] -> Char
lookUp ch [] = ch
lookUp ch (x:xs)
  | fst x == ch = snd x
  | otherwise = lookUp ch xs

--5
encipher :: Int -> Char -> Char
encipher val ch = lookUp ch (makeKey val)
-- sau encipher val ch = chr (ord 'A' + (ord ch - ord 'A' + val) `mod` (ord 'Z' - ord 'A' + 1) )

--6
normalize :: String -> String
normalize "" = ""
normalize (x:xs)
  | isAlphaNum x = toUpper x : normalize xs
  | otherwise = normalize xs

--7
encipherStr :: Int -> String -> String
encipherStr val sir = faux val (normalize sir)
  where
    faux _ "" = ""
    faux z (x:xs)
      | isAlpha x = encipher z x : faux z xs
      | otherwise = x : faux z xs

--8
reverseKey :: [(Char,Char)] -> [(Char,Char)]
reverseKey sir = [(snd x, fst x) | x <- sir]

--9
decipher :: Int -> Char -> Char
decipher val ch = lookUp ch . reverseKey . makeKey $ val
-- sau decipher val ch = chr (ord 'A' + (ord ch - ord 'A' - val) `mod` (ord 'Z' - ord 'A' + 1) )

decipherStr :: Int -> String -> String
decipherStr _ "" = ""
decipherStr val (x:xs)
  | x == ' ' || isDigit x = x : decipherStr val xs
  | isAlpha x && isUpper x = decipher val x : decipherStr val xs
  | otherwise = decipherStr val xs
