import Control.Monad (ap, liftM)

data Reader env a
  = Reader {runReader :: (env -> a)}

instance Functor (Reader env) where
  -- fmap :: (a -> b)
  --      -> Reader env a
  --      -> Reader env b
  --
  -- have:
  --   f :: a -> b
  --   r :: env -> a
  --   ctx :: env
  --
  -- provide:
  --  b
  fmap = liftM

-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f x =
--   x >>= (\x' -> pure (f x'))

instance Applicative (Reader env) where
  -- pure :: a -> Reader env a
  -- have:
  --   x :: a
  -- provide:
  --   a
  pure x = Reader (\_ -> x)

  -- (<*>) :: Reader env (a -> b)
  --       -> Reader env a
  --       -> Reader env b
  -- have:
  --   r1 :: env -> a -> b
  --   r2 :: env -> a
  --   ctx :: env
  -- provide:
  --   b
  (<*>) = ap

-- ap :: (Monad m) => m (a -> b) -> m a -> m b
-- ap f x =
--   -- f' :: a -> b
--   -- x' :: a
--   f
--     >>= ( \f' ->
--             x >>= (\x' -> pure (f' x'))
--         )

instance Monad (Reader env) where
  {-
    -- (>>=) :: Reader env a
    --       -> (a -> Reader env b)
    --       -> Reader env b
    -- have:
    --   r :: env -> a
    --   f :: a -> Reader env b
    --   ctx :: env
    --   tmp :: a
    --   tmp2 :: Reader env b
    --   tmp3 :: env -> b
    -- provide:
    --   b
    Reader r >>= f =
      Reader
        ( \ctx ->
            let tmp = r ctx
                tmp2 = f tmp
             in case tmp2 of
                  Reader tmp3 ->
                    tmp3 ctx
        )
  -}
  Reader r >>= f =
    Reader $ \ctx ->
      runReader (f (r ctx)) ctx

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Eq, Show)

someTree :: Tree Int
someTree =
  Node
    (Node (Leaf 1) (Leaf 2))
    (Leaf (-3))

--    *
--   / \
--  *   -3
-- 1 2

ask :: Reader env env
ask = Reader (\ctx -> ctx)

local ::
  (env -> env) ->
  Reader env a ->
  Reader env a
local f (Reader r) =
  Reader (\ctx -> r (f ctx))

test :: Reader Int String
test = ask >>= (\x -> pure (show (x + 1)))

intsToBools ::
  Tree Int ->
  Reader Int (Tree Bool)
intsToBools (Leaf x) = do
  --  ask >>= (\depth -> pure (Leaf (x >= depth)))
  depth <- ask
  pure (Leaf (x >= depth))
intsToBools (Node l r) = do
  l' <- local (+ 1) (intsToBools l)
  r' <- local (+ 1) (intsToBools r)
  pure $ Node l' r'

-- Node (Node (Leaf False) (Leaf True))
--      (Leaf False)

data State s a
  = State (s -> (a, s))

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  (<*>) = ap

  -- pure :: a -> State s a
  -- have
  --   x :: a
  --   s :: s
  -- want:
  --  (a,s)
  pure x = State (\s -> (x, s))

instance Monad (State s) where
  -- have:
  --  g :: s -> (a,s)
  --  f :: a -> State s b
  --  s :: s
  --  x :: a
  --  s' :: s
  --  tmp0 :: s -> (b,s)
  --  tmp1 :: (b,s)
  -- want:
  --  (b, s)
  State g >>= f =
    State
      ( \s ->
          let (x, s') = g s
              State tmp0 = f x
              tmp1 = tmp0 s'
           in tmp1
      )

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

runState :: State s a -> s -> (a, s)
runState (State f) s = f s

contrivedFunction ::
  Tree Int ->
  State Int (Tree Int)
contrivedFunction (Leaf x) = do
  counter <- get
  put (counter + x)
  pure $ Leaf (x + counter)
contrivedFunction (Node l r) = do
  l' <- contrivedFunction l
  r' <- contrivedFunction r
  pure $ Node l' r'

contrived2 ::
  Tree Int -> Int -> (Tree Int, Int)
contrived2 (Leaf x) counter =
  (Leaf (x + counter), x + counter)
contrived2 (Node l r) counter =
  let (l', counter') = contrived2 l counter
      (r', counter'') = contrived2 r counter'
   in (Node l' r', counter'')
