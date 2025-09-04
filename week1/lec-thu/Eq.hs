{-
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

Reflexivity
    x == x = True
Symmetry
    x == y = y == x
Transitivity
    if x == y && y == z = True, then x == z = True
Extensionality
    if x == y = True and f is a function whose return type is an instance of Eq,
    then f x == f y = True
Negation
    x /= y = not (x == y)

-}

data Pos1 = Pos1 Integer Integer
  deriving (Show)

-- Lawful
instance Eq Pos1 where
  --  (==) :: Pos1 -> Pos1 -> Bool
  Pos1 x1 y1 == Pos1 x2 y2 =
    x1 == x2 && y1 == y2

data Pos2 = Pos2 Integer Integer
  deriving (Show)

-- Outlaw
instance Eq Pos2 where
  Pos2 x1 y1 == Pos2 x2 y2 =
    x1 == x2 || y1 == y2

data Pos3 = Pos3 Integer Integer
  deriving (Show)

-- Outlaw
instance Eq Pos3 where
  Pos3 _x1 _y1 == Pos3 _x2 _y2 =
    True

data Pos4 = Pos4 Integer Integer
  deriving (Show)

-- Outlaw
instance Eq Pos4 where
  Pos4 _x1 _y1 == Pos4 _x2 _y2 =
    False

data Pos5 = Pos5 (Bool -> Integer)

instance Eq Pos5 where
  Pos5 f1 == Pos5 f2 =
    f1 False == f2 False
      && f1 True == f2 True
