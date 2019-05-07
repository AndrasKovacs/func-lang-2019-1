{-# language GeneralizedNewtypeDeriving, LambdaCase #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Prelude hiding (EQ)
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Char

newtype Parser a = Parser {runParser :: StateT String Maybe a}
  deriving (Functor, Applicative, Monad, Alternative, MonadState String)

eof :: Parser ()
eof = get >>= \case
  [] -> pure ()
  _  -> empty

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = get >>= \case
  c:cs | f c -> c <$ put cs
  _ -> empty

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

char :: Char -> Parser Char
char c = satisfy (==c)

char' :: Char -> Parser Char
char' c = char c <* ws

string :: String -> Parser String
string = traverse char

string' :: String -> Parser String
string' s = string s <* ws

ws :: Parser ()
ws = () <$ many (satisfy isSpace)

run :: Parser a -> String -> Maybe (a, String)
run p s = runStateT (runParser p) s

-- 1. feladat: untyped lambda calculus parsing
------------------------------------------------------------
{-

példák:

  lam x. x
  lam x. lam y. x
  lam f. lam g. lam x. f (g x)
  (lam x. x) (lam x. x)

- lambda: "lam"
- változók: csak betűből áll, nem lehet "lam"
- whitespace bárhol lehet
- zárójelezés standard
- feladat része: adjuk meg a kifejezések adattípusát

data Tm = ...

lam lam. lam

-}


-- parser

data Tm
  = Var String
  | Lam String Tm
  | App Tm Tm
  | Int Int
  | Add Tm Tm
  | Bool Bool
  | ITE Tm Tm Tm
  | LTE Tm Tm
  | EQ Tm Tm
  | Let String Tm Tm
  deriving Show

-- if x then y else z

keywords :: [String]
keywords = ["lam", "if", "then", "else", "true", "false", "let", "in"]

ident :: Parser String
ident = do
  s <- some (satisfy isLetter) <* ws
  if elem s keywords
    then empty
    else pure s

numeral :: Parser Int
numeral = read <$> (some (satisfy isDigit) <* ws)

bool :: Parser Tm
bool = Bool <$> ((True <$ string' "true") <|> (False <$ string' "false"))

atom :: Parser Tm
atom =
       (Var <$> ident)
   <|> (Int <$> numeral)
   <|> bool
   <|> (char' '(' *> tm <* char' ')')

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- két infixl operátor: + és függvényapplikáció
-- kettö közül applikáció erősebb

------------------------------------------------------------

apps :: Parser Tm
apps = foldl1 App <$> some atom

adds :: Parser Tm
adds = foldl1 Add <$> sepBy1 apps (char' '+')

lte :: Parser Tm
lte = LTE <$> adds <*> (string' "<=" *> adds)

eq :: Parser Tm
eq = EQ <$> adds <*> (string' "==" *> adds)

lam :: Parser Tm
lam = Lam <$> (string' "lam" *> ident <* char' '.')
          <*> tm

ite :: Parser Tm
ite = ITE <$> (string' "if" *> tm)
          <*> (string' "then" *> tm)
          <*> (string' "else" *> tm)

let' :: Parser Tm
let' = Let <$> (string' "let" *> ident <* string' "=")
           <*> tm
           <*> (string' "in" *> tm)

------------------------------------------------------------

tm :: Parser Tm
tm = lam <|> ite <|> let' <|> lte <|> eq <|> adds

prog :: Parser Tm
prog = ws *> tm <* eof




-- kiértékelés
------------------------------------------------------------

-- untyped LC:
data Val =
    VLam {app :: Val -> Val}
  | VInt Int
  | VBool Bool

instance Show Val where
  -- show VLam{} = "<function>"
  show (VLam _)  = "<function>"
  show (VInt n)  = show n
  show (VBool b) = show b

------------------------------------------------------------

-- mese: zárt kifejezések kiértékelése (GHC-hez hasonló nagyon
--       leegyszerűsítve)
eval :: [(String, Val)] -> Tm -> Val
eval env = \case
  Bool b -> VBool b
  Int n  -> VInt n

  Var x -> case lookup x env of
             Just v -> v
             _      -> error "variable not in scope"

  Lam x t -> VLam (\v -> eval ((x, v):env) t) -- higher-order abstract syntax
  App t u -> case eval env t of
               VLam f -> f (eval env u)
               _      -> error "can only apply a function"
  Add t u -> case (eval env t, eval env u) of
               (VInt n, VInt m) -> VInt (n + m)
               _ -> error "can only add numbers"

  ITE t u v -> case eval env t of
                 VBool True  -> eval env u
                 VBool False -> eval env v
                 _           -> error "expected Bool"

  LTE t u -> case (eval env t, eval env u) of
               (VInt n, VInt m) -> VBool (n <= m)
               _ -> error "expected numbers"
  EQ t u -> case (eval env t, eval env u) of
               (VInt n, VInt m) -> VBool (n == m)
               (VBool b1, VBool b2) -> VBool (b1 == b2)
               _ -> error "can only compare values of the same type"

  Let x t u -> let tVal = eval env t in eval ((x, tVal):env) u

expr :: Tm
expr = (Lam "x" $ Lam "y" $ Add (Var "x") (Var "y"))
      `App` Int 100
      `App` Int 200

p1 :: String
p1 = unlines [
  "let x = 10 in",
  "let y = 20 in",
  "let foo = lam x. lam y. x + x + y in",
  "let comp = lam f. lam g. lam x. f (g x) in",
  "comp (lam x. x) (foo 10) x"
  ]

interp :: String -> Maybe Val
interp src = do
  (e, _) <- run prog src
  pure (eval [] e)
