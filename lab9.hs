--Ex1a)
sfChar :: Char -> Bool
sfChar x = x `elem` ".?!:"

nrPropRec :: String -> Int
nrPropRec "" = 0
nrPropRec (x:xs)
  | sfChar x = 1 + nrPropRec xs
  | otherwise = nrPropRec xs

--Ex1a)
nrPropSelect :: String -> Int
nrPropSelect text = length [x | x<-text, sfChar x]

--Ex2
liniiN :: [[Int]] -> Int -> Bool
liniiN mat n = and [and (map (\x -> x>0) y) | y <- ( filter (\x -> length x == n) mat )]

--Ex3
data Punct = Pt [Int]
            deriving Show
(+++) :: Punct -> Punct -> Punct
(+++) (Pt a) (Pt b) = Pt (a ++ b)

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
      toArb :: a -> Arb
      fromArb :: Arb -> a

instance ToFromArb Punct where
  toArb (Pt []) = Vid
  toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
  fromArb Vid = Pt []
  fromArb (F x) = Pt [x]
  fromArb (N x y) = (fromArb x) +++ (fromArb y) --operator definit de mine mai sus
