import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (ap, liftM)
import Data.Char (isDigit, isSpace, ord)

readInteger :: String -> Int
readInteger s = loop 0 s
  where
    loop acc (c : cs) =
      loop
        (10 * acc + (ord c - ord '0'))
        cs
    loop acc [] = acc

readInteger2 :: String -> Maybe Int
readInteger2 "" = Nothing
readInteger2 s = loop 0 s
  where
    loop acc (c : cs) =
      if isDigit c
        then
          loop
            (10 * acc + (ord c - ord '0'))
            cs
        else Nothing
    loop acc [] = Just acc

readTwoIntegers ::
  String ->
  Maybe (Int, Int)
readTwoIntegers s = loop1 0 s
  where
    loop1 _ "" = Nothing
    loop1 acc (c : cs) =
      if isDigit c
        then
          loop1
            (10 * acc + (ord c - ord '0'))
            cs
        else
          if c == ' '
            then loop2 (acc, 0) cs
            else Nothing
    loop2 (first, second) (c : cs) =
      if isDigit c
        then
          loop2
            ( first,
              second * 10 + (ord c - ord '0')
            )
            cs
        else Nothing
    loop2 (first, second) "" =
      Just (first, second)

readInteger3 ::
  String ->
  Maybe (Int, String)
readInteger3 s =
  case s of
    (c : cs) ->
      if isDigit c
        then loop (ord c - ord '0') cs
        else Nothing
    _ -> Nothing
  where
    loop acc "" = Just (acc, "")
    loop acc (c : cs) =
      if isDigit c
        then
          loop
            (acc * 10 + (ord c - ord '0'))
            cs
        else Just (acc, c : cs)

readSpace :: String -> Maybe ((), String)
readSpace (' ' : cs) = Just ((), cs)
readSpace _ = Nothing

readTwoIntegers2 ::
  String ->
  Maybe ((Int, Int), String)
readTwoIntegers2 s = do
  (x, s') <- readInteger3 s
  ((), s'') <- readSpace s'
  (y, s''') <- readInteger3 s''
  Just ((x, y), s''')

data Parser a
  = Parser
  { unParser ::
      (Int, String) ->
      Either (Int, String) (a, (Int, String))
  }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  -- pure :: a -> Parser a
  -- have:
  --  x :: a
  --  s :: String
  -- want
  --  Maybe (a, String)
  pure x = Parser (\s -> Right (x, s))
  (<*>) = ap

instance Monad Parser where
  {-
    Parser g >>= f =
      Parser
        ( \s -> case g s of
            Nothing -> Nothing
            Just (x, s') ->
              case unParser (f x) s' of
                Nothing -> Nothing
                Just (y, s'') -> Just (y, s'')
        )
  -}
  p >>= f =
    Parser $ \s -> do
      (x, s') <- unParser p s
      unParser (f x) s'

instance MonadFail Parser where
  -- fail :: String -> Parser a
  fail e = Parser $ \(cnt, _) -> Left (cnt, e)

instance Alternative Parser where
  -- (<|>) :: Parser a
  --         -> Parser a
  --         -> Parser a
  -- have:
  --   p1 :: Parser a
  --   p2 :: Parser a
  -- want
  --   Parser a
  p1 <|> p2 = Parser $ \s ->
    case unParser p1 s of
      Left _ -> unParser p2 s
      Right (x, s') -> pure (x, s')

  -- empty :: Parser a
  empty = fail "empty"

(<?>) :: Parser a -> String -> Parser a
p <?> e = Parser $ \(cnt, s) ->
  case unParser p (cnt, s) of
    Left _ -> Left (cnt, e)
    Right (x, s') -> Right (x, s')

many :: Parser a -> Parser [a]
many p = lhs <|> rhs
  where
    lhs = do
      x <- p
      xs <- many p
      pure $ x : xs
    rhs = pure []

some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- many p
  pure $ x : xs

next :: Parser Char
next = Parser $ \(cnt, s) ->
  case s of
    [] -> Left (cnt, "end of input")
    c : cs -> Right (c, (cnt + 1, cs))

eof :: Parser ()
eof = Parser $ \(cnt, s) ->
  case s of
    "" -> Right ((), (cnt, ""))
    _ -> Left (cnt, "expected EOF")

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- next
  if p c
    then pure c
    else fail "does not satisfy"

parseDigit :: Parser Int
parseDigit = do
  c <- satisfy isDigit <?> "expected digit"
  pure $ ord c - ord '0'

parseInteger :: Parser Int
parseInteger = do
  xs <- some parseDigit
  pure $ loop 0 xs
  where
    loop acc (d : ds) =
      loop (acc * 10 + d) ds
    loop acc [] = acc

parseSpace :: Parser ()
parseSpace =
  next
    >>= ( \c ->
            if isSpace c
              then pure ()
              else fail "expected space"
        )

parseSpaces :: Parser ()
parseSpaces = do
  _ <- many parseSpace
  pure ()

parseTwoIntegers ::
  Parser (Int, Int)
parseTwoIntegers = do
  x <- parseInteger <?> "expected first integer"
  parseSpaces
  y <- parseInteger <?> "expected second integer"
  pure (x, y)

runParser ::
  Parser a ->
  String ->
  Either String a
runParser p s = case unParser p (0, s) of
  Left (cnt, err) ->
    Left $
      "After reading "
        ++ show cnt
        ++ " characters\n:"
        ++ err
  Right (x, _) -> Right x
