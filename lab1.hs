myInt = 55555555555555555555555555555555555555555555555555555555555
double :: Integer -> Integer
double x = x+x
triple x = double x + x


maxim :: Integer -> Integer -> Integer
maxim x y =
    if (x > y)
      then x
      else y

--ex1
patrate :: Integer -> Integer -> Integer
patrate x y =
  let u = x*x
      z = y*y
  in u+z

--ex2
paritate :: Integer -> String
paritate x =
  if x `mod` 2 == 1
    then "impar"
    else "par"

--ex3
factorial :: Integer -> Integer
factorial x = product[1..x]

--ex4
verif :: Integer -> Integer -> Bool
verif x y =
  if x > (2*y)
    then True
    else False
