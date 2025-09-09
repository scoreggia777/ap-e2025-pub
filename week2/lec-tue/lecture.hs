import Control.Monad (filterM)

type Name = String

type Age = Int

data Country
  = Denmark
  | Sweden
  | DPRK
  | Latveria
  deriving (Show, Eq)

votingAge :: Country -> Maybe Age
votingAge Denmark = Just 18
votingAge Sweden = Just 18
votingAge DPRK = Just 17
votingAge Latveria = Nothing

ages :: Name -> Maybe Age
ages "Troels" = Just 37
ages "Waleed" = Just 28
ages "Toxo" = Just 7
ages _ = Nothing

mayVote0 ::
  (Name -> Maybe Age) ->
  Name ->
  Maybe Bool
mayVote0 getAge name =
  case getAge name of
    Nothing -> Nothing
    Just x -> Just (x >= 18)

mayVote1 ::
  (Name -> Maybe Age) ->
  Name ->
  Maybe Bool
mayVote1 getAge name =
  (>= 18) <$> getAge name

mayVote2 ::
  (Functor f) =>
  (Name -> f Age) ->
  Name ->
  f Bool
mayVote2 getAge name =
  (>= 18) <$> getAge name

mayVote3 ::
  (Applicative f) =>
  (Name -> f Age) ->
  (Country -> f Age) ->
  Name ->
  Country ->
  f Bool
mayVote3 getAge getMinAge name country =
  let -- x :: f Age
      x = getAge name
      -- y :: f Age
      y = getMinAge country
      -- z :: f (Age -> Bool)
      z =
        fmap
          ( \x' ->
              (\minAge -> x' >= minAge)
          )
          x
      -- t :: f Bool
      t =
        z <*> y
   in t

mayVote4 ::
  (Applicative f) =>
  (Name -> f Age) ->
  (Country -> f Age) ->
  Name ->
  Country ->
  f Bool
mayVote4 getAge getMinAge name country =
  (>=) <$> getAge name <*> getMinAge country

-- class Applicative f where
--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b

-- pure :: a -> Maybe a
-- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b

applyMaybe ::
  Maybe (a -> b) ->
  Maybe a ->
  Maybe b
applyMaybe (Just f) (Just x) = Just (f x)
applyMaybe _ _ = Nothing

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b

bindMaybe ::
  Maybe a ->
  (a -> Maybe b) ->
  Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

findVoter ::
  (Monad m) =>
  (Country -> m Age) ->
  (Name -> m Age) ->
  Country ->
  [Name] ->
  m [Name]
findVoter getMinAge getAge country persons =
  case persons of
    [] -> pure []
    person : remaining ->
      let -- tmp :: m Bool
          tmp =
            (>=)
              <$> getAge person
              <*> getMinAge country
          -- g :: Bool -> m [Name]
          g True =
            -- person :: Name
            -- m [Name]
            (person :)
              <$> findVoter
                getMinAge
                getAge
                country
                remaining
          g False =
            findVoter
              getMinAge
              getAge
              country
              remaining
       in tmp >>= g

-- m >>= (\x -> f x)
--
-- In do notation:
--
-- do x <- m
--    f x
--
-- m >>= (\x ->
--    f x >>= (\y -> ... g y)
--
-- do x <- m
--    y <- f x
--    ...

findVoterDo ::
  (Monad m) =>
  (Country -> m Age) ->
  (Name -> m Age) ->
  Country ->
  [Name] ->
  m [Name]
findVoterDo getMinAge getAge country persons =
  case persons of
    [] -> pure []
    person : remaining -> do
      -- x :: Bool
      x <-
        (>=)
          <$> getAge person
          <*> getMinAge country
      -- rest :: [Name]
      rest <-
        findVoterDo
          getMinAge
          getAge
          country
          remaining
      pure $
        if x
          then person : rest
          else rest

findVoterFilterM ::
  (Monad m) =>
  (Country -> m Age) ->
  (Name -> m Age) ->
  Country ->
  [Name] ->
  m [Name]
findVoterFilterM getMinAge getAge country persons =
  filterM mayVote persons
  where
    mayVote person =
      (>=)
        <$> getAge person
        <*> getMinAge country
