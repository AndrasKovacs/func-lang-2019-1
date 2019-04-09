
{-# language DeriveFunctor #-}

import Control.Monad
import Control.Applicative
import Data.Char

-- Parser monád
------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
--  deriving (Functor)

-- State Functor instance-hoz hasonló
instance Functor Parser where
  fmap f (Parser g) = Parser $ \s -> case g s of
                                       Just (a, s') -> Just (f a, s')
                                       Nothing      -> Nothing

instance Applicative Parser where
  pure  = return
  (<*>) = ap

-- State + hibalehetőség
instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)

  -- lefut az első Parser, aztán a második,
  -- csak akkor Just-ot, ha mindkét Parser Just-ot ad
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

-- konkrét karaktert olvas
char :: Char -> Parser Char
char c = Parser $ \s -> case s of
           c':cs | c == c' -> Just (c, cs)
           _ -> Nothing

-- üres inputon sikeres csak
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- példa eof használatára: pontosan egy karaktert olvasunk
-- runParser (char 'x' <* eof)

-- (<*) :: Applicative f => f a -> f b -> f a
-- (*>) :: Applicative f => f a -> f b -> f b

-- import Control.Applicative
-- (superclass: Applicative)
-- (Alternative: bármely a-ra, Monoid (f a))
instance Alternative Parser where
  -- empty :: Parser a
  -- rögtön hibát dob
  empty = Parser (const Nothing)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  -- ha a bal Parser nem sikeres, akkor megpróbálja a jobb
  -- Parser-t
  Parser f <|> Parser g = Parser $ \s ->
    case f s of
      Nothing -> g s
      x       -> x

-- példa:
lowerCaseLetter :: Parser Char
lowerCaseLetter = foldr (<|>) empty (map char ['a'..'z'])
   -- char 'a' <|> char 'b' <|> char 'c' .....

-- két standard Control.Applicative függvény: many, some
-- many :: Alternative f => f a -> f [a]
-- some :: Alternative f => f a -> f [a]

-- példák:
-- egy vagy több lowerCaseLetter olvasása: some
-- runParser (some lowerCaseLetter) "aaaaa"  -- OK
-- runParser (some lowerCaseLetter) ""       -- fail

-- nulla vagy több dolog olvasás: many
-- runParser (many lowerCaseLetter) "aaaa"   -- OK
-- runParser (many lowerCaseLetter) ""       -- OK

-- tipp: legegyszerűbb kölcsönös rekurzív some és many definíció

some' :: Alternative f => f a -> f [a]
some' fa = (:) <$> fa <*> many' fa -- fa-t olvasunk, utána pedig 0 vagy több fa-t

many' :: Alternative f => f a -> f [a]
many' fa = some' fa <|> pure []

-- some, many, <|>, char : regurális kifejezést olvasni
-- many p: p*
-- some p: p+

-- példa: many (char 'a' <|> char 'b')
-- runParser (many (char 'a' <|> char 'b')) "aaabbbbabcabab" -- OK
-- runParser (many (char 'a' <|> char 'b') <* eof) "aaabbbbabcabab" -- FAIL

string :: String -> Parser String
string = traverse char

-- példa:
-- runParser (many (string "ab") *> string "foo" *> eof) "ababfoo" -- OK

-- klasszikus monadikus parser példa:
-- n darab 'a' karakter után n darab 'b' karaktert (n tesztőleges term. szám)
-- OK: "aabb",  "aaabbb",   "ab",   ""

anbn :: Parser Int -- adjuk vissza az olvasott 'a'-k számát
anbn = do
  -- először olvassunk akárhány a-t
  -- utána, ahány a-t olvastunk, annyi b-t olvassunk
  as <- many (char 'a')
  let l = length as
  bs <- replicateM l (char 'b')
  pure l

-- token parsing : whitespace kezelése parsoláskor

-- példa túl szigorú parsoálsra

int :: Parser Int
int = read <$> some (foldr (<|>) empty $ map char ['0'..'9'])

parens :: Parser a -> Parser a
parens pa = char '(' *> pa <* char ')'  -- azt az értéket adjuk vissza,
                                        -- amire a csőrök mutatnak

intPair :: Parser (Int, Int)
intPair =
  parens $ do
    n1 <- int
    char ','
    n2 <- int
    pure (n1, n2)

-- túl szigorú intPair




satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  c:cs | p c -> Just (c, cs)
  _ -> Nothing

-- char c = satisfy (==c)

-- import Data.Char

ws :: Parser ()
ws = many (satisfy isSpace) *> pure ()


-- token parser: olyan parser, ami maga után megeszi a whitespace-eket
symbol :: String -> Parser String
symbol str = string str <* ws

intTok :: Parser Int
intTok = int <* ws

intPair' :: Parser (Int, Int)
intPair' = do
  symbol "("
  n1 <- intTok
  symbol ","
  n2 <- intTok
  symbol ")"
  pure (n1, n2)

-- konvenció: minden token parser maga után olvas ws-t
-- egész program parsolásánál ws-el kezdünk, eof-al fejezünk be

-- pl: ha az egész program csak egy Int pár

prog :: Parser (Int, Int)
prog = ws *> intPair' <* eof
