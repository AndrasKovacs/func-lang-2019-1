{-# language DeriveFunctor #-}

import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
 deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  Parser f <|> Parser g = Parser $ \s ->
    case f s of
      Nothing -> g s
      x       -> x

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
  c:cs -> Just (c, cs)
  []   -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- anyChar
  if f c then pure c else empty

char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string = traverse char

ws :: Parser ()
ws = () <$ many (satisfy isSpace)

--------------------------------------------------------------------------------

-- Írj egy parser-t, ami beolvas egy vagy több `a`-t `b`-vel
-- elválasztva, és visszaadja a beolvasott `a`-kat listában.
-- Pl. runParser (sepBy1 (char 'x') (char 'y')) "xyxyx" == "xxx"
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 = undefined

-- Írj egy parser-t, ami mindig sikeres, és "Just a"-t ad vissza, ha
-- az input parser sikeres, egyébként Nothing-ot.
optional :: Parser a -> Parser (Maybe a)
optional = undefined


-- Írj parser-t, ami úgy működik, mint a sepBy1, viszont végül opcionálisan
-- beolvas egy adott parsert.
sepEndBy1 :: Parser a -> Parser b -> Parser end -> Parser [a]
sepEndBy1 = undefined

-- Írj egy parser-t, ami számjegyek vesszővel elválasztott listáit olvassa be.
-- Példák:
--   [1, 4, 5, 1]
--   [  0  , 2  ]
--   []
--   [     ]
-- Használj ws-t a szóközök olvasásához.



-- Add vissza a számjegyek mint karakterek listáját.
digitList :: Parser String
digitList = undefined

-- Módosítsd a digitList parser-t úgy, hogy az utolsó listaelem után
-- megengedjen egy vesszőt.
digitListTrailingComma :: Parser String
digitListTrailingComma = undefined

-- Írj egy parser-t, ami helyes zárójelezéseket olvas.
-- Példák:
--   "()"
--   ""
--   "()()()"
--   "(())()"
--   "((()()))"
parenExpr :: Parser ()
parenExpr = undefined


-- Zárójelek + számliterálok + összeadás
-- lehet whitespace is minden között

-- (12 + 0)
--  10 + 10
--  10 + 10 + 10 + 10

expr :: Parser ()
expr = undefined
