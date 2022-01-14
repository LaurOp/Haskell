newtype Any = Any { getAny :: Bool }
  deriving (Eq, Show)
instance Semigroup Any where
  Any x <> Any y = Any (x || y)
instance Monoid Any where
  mempty = Any False
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem c = getAny . (foldMap (Any . (== c)))


newtype All = All { getAll :: Bool }
  deriving (Eq, Show)
instance Semigroup All where
  All x <> All y = All (x && y)
instance Monoid All where
  mempty = All True
null :: (Foldable t) => t a -> Bool
null = getAll . (foldMap (All . (\x -> length x == 0)))
ls1 = ["", "", "2"]

-- length :: (Foldable t) => t a -> Int
-- length = undefined


toList :: (Foldable t) => t a -> [a]
toList = undefined


fold :: (Foldable t, Monoid m) => t m -> m
fold = undefined
