{-
class Semigroup a where
  (<>) :: a -> a -> a

Laws:

Associativity
    x <> (y <> z) = (x <> y) <> z

class Semigroup a => Monoid a where
  mempty :: a

Laws:

Right identity
    x <> mempty = x
Left identity
    mempty <> x = x

-}

data Add = Add Integer
  deriving (Show)

-- Lawful
instance Semigroup Add where
  -- (<>) :: Add -> Add -> Add
  Add x <> Add y = Add (x + y)

instance Monoid Add where
  mempty = Add 0

data Sub = Sub Integer
  deriving (Show)

-- Outlaw
instance Semigroup Sub where
  -- (<>) :: Sub -> Sub -> Sub
  Sub x <> Sub y = Sub (x - y)

data Mul = Mul Integer
  deriving (Show)

-- Lawful
instance Semigroup Mul where
  -- (<>) :: Mul -> Mul -> Mul
  Mul x <> Mul y = Mul (x * y)

instance Monoid Mul where
  mempty = Mul 1
