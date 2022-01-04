--  #*#*#*#**##**#*#*#*#*##**#*#
--  *#*    MARE              *#*
--  *#*    DISCLAIMER CA     *#*
--  *#*  AM REZOLVAT CU      *#*
--  *#*    RECORDS, NU CU    *#*
--  *#*  PATTERN MATCHING!!  *#*
--  #*#*#*#**##**#*#*#*#*##**#*#

data Fruct = Mar {tip::String, viermi::Bool} | Portocala {tip::String, felii::Int}
listaFructe = [Mar "Ionatan" False, Portocala "Sanguinello" 10, Portocala "Valencia" 22, Mar "Golden Delicious" True, Portocala "Sanguinello" 15, Portocala "Moro" 12, Portocala "Tarocco" 3, Portocala "Moro" 12, Portocala "Valencia" 2, Mar "Golden Delicious" False, Mar "Golden" False, Mar "Golden" True]

--1a
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala x y)
  | x == "Tarocco" || x == "Moro" || x == "Sanguinello" = True
  | otherwise = False

--1b
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x:xs)
  | ePortocalaDeSicilia x = felii x + nrFeliiSicilia xs
  | otherwise = nrFeliiSicilia xs

--1c
nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (x:xs)
  | mar x && viermi x = 1 + nrMereViermi xs
  | otherwise = nrMereViermi xs
  where
    mar (Mar _ _) = True
    mar (Portocala _ _) = False

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show
--2a
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

--2b
rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine x y) = Just y

--3a
data Linie = L {elemente::[Int]}
  deriving Show
data Matrice = M {linii::[Linie]}
  deriving Show

verifica :: Matrice -> Int -> Bool
verifica mat nr = and [foldr (+) 0 (elemente x) == nr | x <- linii mat]

--3b
doarPozN :: Matrice -> Int -> Bool
doarPozN mat nr = and [length (filter (>0) (elemente x)) == nr | x <- linii mat, length (elemente x) == nr]

--3c
corect :: Matrice -> Bool
corect mat = func [length (elemente x) | x <- linii mat ]
  where
    func [] = True
    func [_] = True
    func (x:xs)
      | x == head xs = func xs
      | otherwise = False
