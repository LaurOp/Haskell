rev :: [a] -> [a]
rev sir = foldr (op) [] sir
  where
    op :: a -> [a] -> [a]
    op x y = y ++ [x]

prefixes :: [a] -> [[a]]
prefixes sir = foldr (op) [] (zip sir [1..])
  where
    op :: (a,Int) -> [[a]] ->[[a]]
    op x y = y ++ (take (snd x) sir)
