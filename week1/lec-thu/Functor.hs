{-
class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

Identity
    fmap id == id
Composition
    fmap (f . g) == fmap f . fmap g
-}

data Box a = Box a
  deriving (Show)

instance (Eq a) => Eq (Box a) where
  Box x == Box y = x == y

instance Functor Box where
  -- fmap :: (a -> b) -> Box a -> Box b
  -- need: b
  -- have:
  --  f :: a -> b
  --  x :: a
  fmap f (Box x) = Box (f x)

-- Same as Haskell's Maybe
data Opt a = Some a | None
  deriving (Show, Eq)

instance Functor Opt where
  -- fmap :: (a -> b) -> Opt a -> Opt b
  fmap _ None = None
  fmap f (Some x) = Some (f x)

data Fun a b = Fun (a -> b)

instance Functor (Fun c) where
  -- fmap :: (a -> b) -> Fun c a -> Fun c b
  -- have:
  --  f :: a -> b
  --  g :: c -> a
  fmap f (Fun g) = Fun (\x -> f (g x))
