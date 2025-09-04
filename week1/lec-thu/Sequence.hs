module Sequence
  ( Sequence,
    singleton,
    toList,
    fromList,
  )
where

data Sequence a
  = Empty
  | Single a
  | Concat (Sequence a) (Sequence a)
  deriving (Show)

instance (Eq a) => Eq (Sequence a) where
  xs == ys = toList xs == toList ys

singleton :: a -> Sequence a
singleton x = Single x

instance Semigroup (Sequence a) where
  xs <> ys = Concat xs ys

instance Monoid (Sequence a) where
  mempty = Empty

toList :: Sequence a -> [a]
toList Empty = []
toList (Single x) = [x]
toList (Concat xs ys) = toList xs <> toList ys

fromList :: [a] -> Sequence a
fromList [] = Empty
fromList (x : xs) = singleton x <> fromList xs

instance Functor Sequence where
  -- fmap :: (a -> b) -> Sequence a -> Sequence b
  fmap _ Empty = Empty
  fmap f (Single x) = Single (f x)
  -- have:
  --  f :: a -> b
  --  xs :: Sequence a
  --  ys :: Sequence a
  -- Need:
  --  Sequence b
  fmap f (Concat xs ys) =
    Concat (fmap f xs) (fmap f ys)

instance Foldable Sequence where
  -- foldr :: (a -> b -> b) -> b -> Sequence a -> b
  foldr _ acc Empty = acc
  foldr f acc (Single x) = f x acc
  foldr f acc (Concat xs ys) =
    foldr f (foldr f acc ys) xs

normalise ::
  (Ord a, Floating a, Functor f, Foldable f) =>
  f a ->
  f a
normalise f =
  let m = maximum f
   in fmap (\x -> x / m) f

average :: (Fractional a, Foldable t) => t a -> a
average f = sum f / fromIntegral (length f)

data Vec3 a = Vec3 a a a
  deriving (Eq, Ord, Show)

instance Functor Vec3 where
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Foldable Vec3 where
  foldr f acc (Vec3 x y z) =
    x `f` (y `f` (z `f` acc))
