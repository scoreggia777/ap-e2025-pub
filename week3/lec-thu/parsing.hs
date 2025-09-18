import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (ap, liftM, void)
import Data.Char (isAlpha, isDigit, isSpace, ord)

data Parser a
  = Parser
  { unParser ::
      (Int, String) ->
      Either (Int, String) (a, (Int, String))
  }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure x = Parser (\s -> Right (x, s))
  (<*>) = ap

instance Monad Parser where
  p >>= f =
    Parser $ \s -> do
      (x, s') <- unParser p s
      unParser (f x) s'

instance MonadFail Parser where
  fail e = Parser $ \(cnt, _) -> Left (cnt, e)

instance Alternative Parser where
  p1 <|> p2 = Parser $ \s ->
    case unParser p1 s of
      Left _ -> unParser p2 s
      Right (x, s') -> pure (x, s')

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

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \(cnt, s) ->
  case unParser p (cnt, s) of
    Left _ -> Right ((), (cnt, s))
    Right (_, _) ->
      Left (cnt, "expected failure")

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

spaces :: Parser ()
spaces = void $ many $ satisfy isSpace

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  spaces
  pure x

lComma :: Parser ()
lComma = lexeme $ void $ satisfy (== ',')

lOpenBracket :: Parser ()
lOpenBracket = lexeme $ void $ satisfy (== '[')

lCloseBracket :: Parser ()
lCloseBracket = lexeme $ void $ satisfy (== ']')

lInt :: Parser Int
lInt = lexeme parseInteger

pIntsSepByCommas :: Parser [Int]
pIntsSepByCommas =
  nonemptyCase <|> emptyCase
  where
    emptyCase = pure []
    nonemptyCase = do
      x <- lInt
      xs <- many p
      pure $ x : xs
    p = do
      lComma
      x <- lInt
      pure x

pIntList :: Parser [Int]
pIntList = do
  lOpenBracket
  xs <- pIntsSepByCommas
  lCloseBracket
  pure xs

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

intListFromString :: String -> Either String [Int]
intListFromString s =
  case runParser (spaces *> pIntList <* eof) s of
    Left e -> Left e
    Right x -> Right x

--
-- Exp ::= "true"
--       | "false"
--       | id
--       | Exp "and" Exp
--       | Exp "or" Exp
--       | "not" Exp
--
-- tokens separated by whitespace.
--
-- identifiers are one or more alphabetic characters.
-- [a-zA-Z]+.

data Exp
  = Constant Bool
  | Var String
  | And Exp Exp
  | Or Exp Exp
  | Not Exp
  deriving (Show)

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ do
  void $ mapM (\c -> satisfy (== c)) s
  notFollowedBy $ satisfy isAlpha
  pure ()

pTrue, pFalse, pAnd, pOr, pNot :: Parser ()
pTrue = lKeyword "true"
pFalse = lKeyword "false"
pAnd = lKeyword "and"
pOr = lKeyword "or"
pNot = lKeyword "not"

keywords :: [String]
keywords = ["true", "false", "and", "or", "not"]

lId :: Parser String
lId = lexeme $ do
  s <- some $ satisfy isAlpha
  if s `elem` keywords
    then
      fail "cannot use keyword as identifer"
    else pure s

-- Removing left recursion:
--
-- Exp ::= "true"
--       | "false"
--       | id
--       | Exp "and" Exp
--       | Exp "or" Exp
--       | "not" Exp
--
--  ======
--
-- right associative:
--
-- Exp0 ::= "true" Exp1
--       | "false" Exp1
--       | id Exp1
--
-- Exp1 ::= \epsilon
--        | "and" Exp0
--        | "or" Exp0
--
-- left associative:
--
-- Atom ::= "true"
--        | "false"
--        | id
--        | "(" Exp0 ")"
--
-- Exp0 ::= Atom Exp1
--
-- Exp1 ::= \epsilon
--        | "and" Atom Exp1
--        | "or" Atom Exp1
--
--  ===
--
-- with operator priorities
--
-- Atom ::= "true"
--        | "false"
--        | '(' Exp0 ')'
--        | id
--
-- Exp1 ::= Atom Exp1'
-- Exp1' ::= \epsilon
--         | "and" Atom Exp1'
--
-- Exp0 ::= Exp1 Exp0'
-- Exp0' ::= \epsilon
--         | "or" Exp1 Exp0'

lClosePar, lOpenPar :: Parser ()
lClosePar = lexeme $ void $ satisfy (== ')')
lOpenPar = lexeme $ void $ satisfy (== '(')

parens :: Parser a -> Parser a
parens p = do
  lOpenPar
  x <- p
  lClosePar
  pure x

pAtom :: Parser Exp
pAtom =
  trueCase
    <|> falseCase
    <|> idCase
    <|> parenCase
  where
    trueCase = do
      pTrue
      pure $ Constant True
    falseCase = do
      pFalse
      pure $ Constant False
    idCase = do
      x <- lId
      pure $ Var x
    parenCase = parens pExp0

pExp0 :: Parser Exp
pExp0 = do
  e <- pExp1
  chain e
  where
    chain x =
      orCase x <|> pure x
    orCase x = do
      pOr
      y <- pExp1
      chain $ Or x y

pExp1 :: Parser Exp
pExp1 = do
  e <- pAtom
  chain e
  where
    chain x =
      andCase x <|> pure x
    andCase x = do
      pAnd
      y <- pExp1
      chain $ And x y

pExp :: Parser Exp
pExp =
  pAndExp
    <|> pTrueExp
    <|> pFalseExp
    <|> pVarExp
  where
    --    <|> pOrExp

    pTrueExp = do
      pTrue
      pure $ Constant True
    pFalseExp = do
      pFalse
      pure $ Constant False
    pVarExp = do
      x <- lId
      pure $ Var x
    pAndExp = do
      x <- pExp
      pAnd
      y <- pExp
      pure $ And x y

--    pOrExp = undefined
