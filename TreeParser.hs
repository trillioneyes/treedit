module TreeParser where
import Lib hiding (children)
import Text.Read(readMaybe)
import Control.Applicative

data ParseError = ParseError

type Parse a b = [Tree a] -> Either ParseError (b, [Tree a])

consume' :: (a -> [Tree a] -> b) -> Parse a b
consume' f (T x cs:ts) = Right (f x cs, ts)
consume' _ _ = Left ParseError

pop' :: Parse a (Tree a)
pop' (t:ts) = Right (t, ts)
pop' _ = Left ParseError

push' :: Tree a -> Parse a ()
push' t ts = Right ((), t:ts)

empty' :: Parse a ()
empty' [] = Right ((), [])
empty' _ = Left ParseError

newtype Parser a b = Parser { runParser :: Parse a b}

instance Functor (Parser a) where
  fmap f p = Parser (\ts -> do
    (result, rest) <- runParser p ts
    return (f result, rest))

instance Applicative (Parser a) where
  pure x = Parser (\ts -> Right (x, ts))
  p1 <*> p2 = Parser (\ts -> do
    (f, rest) <- runParser p1 ts
    (x, rest') <- runParser p2 rest
    return (f x, rest'))

instance Monad (Parser a) where
  return = pure
  p >>= f = Parser (\ts -> do
    (x, rest) <- runParser p ts
    runParser (f x) rest)

instance Alternative (Parser a) where
  empty = parseFail
  p1 <|> p2 = do
    ts <- remaining
    case runParser p1 ts of
      Right (val, rest) -> leave rest >> return val
      Left _ -> case runParser p2 ts of
        Right (val, rest) -> leave rest >> return val
        Left _ -> parseFail

consume :: (a -> [Tree a] -> b) -> Parser a b
consume = Parser . consume'

parseFail :: Parser a b
parseFail = Parser (const (Left ParseError))

remaining :: Parser a [Tree a]
remaining = Parser (\ts -> Right (ts, []))

leave :: [Tree a] -> Parser a ()
leave ts = Parser (\_ -> Right ((), ts))

pop :: Parser a (Tree a)
pop = Parser pop'

modNext :: (Tree a -> Tree a) -> Parser a (Tree a)
modNext f = do
  t <- f <$> pop
  push t
  return t

peek :: Parser a (Tree a)
peek = modNext id

push :: Tree a -> Parser a ()
push t = Parser (push' t)

tag :: Parser a a
tag = do
  t@(T x _) <- peek
  push t
  return x

child :: Parser a (Tree a)
child = do
  t <- pop
  case t of
    (T x (c:ts)) -> push (T x ts) >> return c
    _ -> parseFail

children :: Parser a [Tree a]
children = do
  T x ts <- pop
  push (T x [])
  return ts

requireMaybe :: Maybe b -> Parser a b
requireMaybe = maybe parseFail return

subParse :: Parser a b -> [Tree a] -> Parser a b
subParse p ts = do
  r <- remaining
  leave ts
  pResult <- p
  leave r
  return pResult

parseInt :: String -> Parser a Int
parseInt = requireMaybe . readMaybe

expect :: Eq a => a -> Parser a b -> Parser a b
expect x p = do
  T x' ts <- pop
  if x' == x
    then subParse p ts
    else parseFail

expect_ :: Parser a b -> Parser a b
expect_ p = do
  cs <- children
  _ <- pop
  subParse p cs

endOfInput :: Parser a ()
endOfInput = Parser empty'

leaf :: Parser a a
leaf = pop >>= (\(T x ts) -> subParse endOfInput ts >> return x)

parse :: Parser a b -> Tree a -> Either ParseError b
parse p t = case runParser p [t] of
  Right (val, _) -> return val
  Left e -> Left e
