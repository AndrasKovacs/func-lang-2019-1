{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Char
import Data.String
import Prelude hiding (EQ, LT)

--------------------------------------------------------------------------------

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

ws :: Parser ()
ws = () <$ many (satisfy isSpace)

char' :: Char -> Parser Char
char' c = satisfy (==c)

char :: Char -> Parser Char
char c = char' c <* ws

string' :: String -> Parser String
string' = traverse char'

string :: String -> Parser String
string s = string' s <* ws

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 pa psep = go =<< pa where
  go acc = do {f <- psep; a <- pa; go (f acc a)} <|> pure acc

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 pa psep = pa >>= go where
  go a = (psep <*> pure a <*> (pa >>= go)) <|> pure a

run :: Parser a -> String -> Maybe (a, String)
run = runStateT . runParser

--------------------------------------------------------------------------------

type Block = [Statement] -- s1 ; s2 ; ... ; sn

data Exp
  = Add Exp Exp         -- e1 + e2         infixl 6 +
  | Mul Exp Exp         -- e1 * e2         infixl 7 *
  | Sub Exp Exp         -- e1 - e2         infixl 6 -
  | EQ  Exp Exp         -- e1 == e2        infix 4 ==
  | LT  Exp Exp         -- e1 < e2         infix 4 <
  | BoolOr Exp Exp      -- e1 || e2        infixr 2 ||
  | BoolAnd Exp Exp     -- e1 && e2        infixr 3 &&
  | BoolNot Exp         -- not e
  | IntLit Int          -- [0-9]+
  | BoolLit Bool        -- true | false
  | ReadInt             -- readInt          beolvas egy Int-et konzolról
  | Var String
  deriving Show

data Statement
  = Assign String Exp      -- x := e
  | While Exp Block        -- while e1 do b end
  | Block Block            -- {b}
  | If Exp Block           -- if e1 then b end
  | IfElse Exp Block Block -- if e1 then b1 else b2 end
  | Print Exp              -- print e
  deriving Show

--------------------------------------------------------------------------------

keywords :: [String]
keywords =
  ["while", "true", "false", "if", "then",
   "else", "end", "not", "print", "readInt"]

pIdent :: Parser String
pIdent = do
  x <- some (satisfy isLetter) <* ws
  when (elem x keywords) empty
  pure x

-- kifejezések
----------------------------------------

pAtom :: Parser Exp
pAtom =
      (Var <$> pIdent)
  <|> (BoolLit <$> ((True <$ string "true") <|> (False <$ string "false")))
  <|> (IntLit . read <$> some (satisfy isDigit) <* ws)
  <|> (char '(' *> pExp <* char ')')
  <|> (ReadInt <$ string "readInt")

pOr  = chainr1 pAnd (BoolOr <$ string "||")
pAnd = chainr1 (pEQLT <|> pSubAdd) (BoolAnd <$ string "&&")

-- left-factoring
pEQLT = do
  e1 <- pSubAdd
  op <- (EQ <$ string "==") <|> (LT <$ char '<')
  e2 <- pSubAdd
  pure (op e1 e2)

-- azonos precedencia kezelése "+" és "-" esetén
pSubAdd = chainl1 pMul ((Add <$ char '+') <|> (Sub <$ char '-'))

pMul = chainl1 pNot (Mul <$ char '*')
pNot = (BoolNot <$> (string "not" *> pAtom)) <|> pAtom

pExp :: Parser Exp
pExp = pOr

----------------------------------------

pBlock :: Parser Block
pBlock = sepBy1 pStatement (char ';')

-- left-factoring
pIfElse :: Parser Statement
pIfElse = do
  e1 <- string "if" *> pExp <* string "then"
  b  <- pBlock
  let p1 = string "end" *> pure (If e1 b)
      p2 = IfElse e1 b <$> (string "else" *> pBlock <* string "end")
  p1 <|> p2

pStatement :: Parser Statement
pStatement =
      (Assign <$> pIdent <*> (string ":=" *> pExp))
  <|> (While <$> (string "while" *> pExp <* string "do") <*> (pBlock <* string "end"))
  <|> (Block <$> (char '{' *> pBlock <* char '}'))
  <|> pIfElse
  <|> (Print <$> (string "print" *> pExp))

pProgram :: Parser Block
pProgram = ws *> pBlock <* eof


-- Feladat: kiértékelés
--------------------------------------------------------------------------------

{-

Példa: olvassunk be egy N számot, majd nyomtassuk az első N darab fibonacci számot ki:

  count := readInt;
  x := 0;
  y := 1;
  i := 0;
  while (i < count) do
    print x;
    tmp := x + y;
    x := y;
    y := tmp;
    i := i + 1
  end

- ReadInt működése: olvassunk be egy sort a konzolról (getLine), majd "read"-el
  parsoljunk Int-re, és adjuk vissza az olvasott értéket.

- Print működése: a "print e" állításnál értékeljük ki az "e" kifejezést, majd
  printeljük ki. A kifejezés értéke lehet Int és Bool is.

- Változók kezelése: a While, If, IfElse és Block konstrukciók belsejében a scope
  *lokális*! Azaz pl. a while testében deklarált változó nem látható a while
  blokkon kívül. Példa: a következő program futásidejű hibát dob, mivel a "print
  y" állításban nincs "y" scope-ban.

    x := 0;
    while x < 100 do
      y := 10;
      x := x + y
    end;
    print y

  A "Block" konstrukció arra használható, hogy lokális változókat vezessünk be
  tetszés szerint:

    x := 0;
    {
       y := 100;
       x := x * y
    };
    print x

  Itt az "y" csak a kapcsos zárójellel határolt blokkban látható.

-}

data Val   = VInt Int | VBool Bool
type Env   = [(String, Val)]
type EvalM = StateT Env (ExceptT String IO)

-- | Evaluate an expression.
evalExp :: Exp -> EvalM Val
evalExp = \case
  Add e1 e2      -> (,) <$> evalExp e1 <*> evalExp e2 >>= \case
                      (VInt n, VInt m) -> pure (VInt (n + m))
                      _                -> throwError "non-number argument in addition"
  Mul e1 e2      -> (,) <$> evalExp e1 <*> evalExp e2 >>= \case
                      (VInt n, VInt m) -> pure (VInt (n * m))
                      _                -> throwError "non-number argument in multiplication"
  Sub e1 e2      -> (,) <$> evalExp e1 <*> evalExp e2 >>= \case
                      (VInt n, VInt m) -> pure (VInt (n - m))
                      _                -> throwError "non-number argument in subtraction"
  EQ e1 e2       -> (,) <$> evalExp e1 <*> evalExp e2 >>= \case
                      (VInt n , VInt m  ) -> pure (VBool (n == m))
                      (VBool b, VBool b') -> pure (VBool (b == b'))
                      _                   -> throwError "differently typed arguments in =="
  LT e1 e2       -> (,) <$> evalExp e1 <*> evalExp e2 >>= \case
                      (VInt n, VInt m) -> pure (VBool (n < m))
                      _                -> throwError "non-number argument in <"
  BoolOr e1 e2   -> (,) <$> evalExp e1 <*> evalExp e2 >>= \case
                      (VBool b, VBool b') -> pure (VBool (b || b'))
                      _                   -> throwError "non-Bool argument in ||"
  BoolAnd e1 e2  -> (,) <$> evalExp e1 <*> evalExp e2 >>= \case
                      (VBool b, VBool b') -> pure (VBool (b && b'))
                      _                   -> throwError "non-Bool argument in &&"
  BoolNot e      -> evalExp e >>= \case
                      VBool b -> pure (VBool (not b))
                      _       -> throwError "non-Bool argument in Boolean negation"
  IntLit n       -> pure (VInt n)
  BoolLit b      -> pure (VBool b)
  ReadInt        -> VInt . read <$> liftIO getLine
  Var x          -> (lookup x <$> get) >>= \case
                      Nothing -> throwError "name not in scope"
                      Just v  -> pure v


-- | Evaluate a statement.
evalStatement :: Statement -> EvalM ()
evalStatement = \case
  Assign x e -> do
    v <- evalExp e
    modify $ \env -> case span ((/=x) . fst) env of
      (as, _:bs) -> as ++ (x, v):bs
      (as, _   ) -> (x, v):as
  While e b ->
    evalExp e >>= \case
      VBool True  -> evalBlock b >> evalStatement (While e b)
      VBool False -> pure ()
      _           -> throwError "non-Bool value in \"while\" condition"
  Block b ->
    evalBlock b
  If e b ->
    evalExp e >>= \case
      VBool True  -> evalBlock b
      VBool False -> pure ()
      _           -> throwError "non-Bool value in \"if\" condition"
  IfElse e b1 b2 ->
    evalExp e >>= \case
      VBool True  -> evalBlock b1
      VBool False -> evalBlock b2
      _           -> throwError "non-Bool value in \"if\" condition"
  Print e ->
    evalExp e >>= \case
      VInt n  -> liftIO $ print n
      VBool b -> liftIO $ print b

-- | Evaluate a block
evalBlock :: Block -> EvalM ()
evalBlock block = do
  env <- get
  mapM_ evalStatement block
  -- we drop all locally declared variables after we exit a block
  modify $ \env' -> drop (length env' - length env) env'

-- | Parse and evaluate a string as a program.
interpret :: String -> IO ()
interpret src = case run pProgram src of
  Nothing     -> putStrLn "parse error"
  Just (p, _) -> do
    runExceptT (runStateT (evalBlock p) []) >>= \case
      Left err -> putStrLn err
      Right _  -> pure ()

-- tests
--------------------------------------------------------------------------------

p1 = interpret $ unlines [
 "x := 0;",
 "{",
 "   y := 100;",
 "   x := x + y",
 "};",
 "print x"
 ]

p2 = interpret $ unlines [
    "x := 0;",
    "while x < 100 do",
    "  y := 10;",
    "  x := x + y",
    "end;",
    "print y"
  ]

p3 = interpret $ unlines [
  "count := readInt;",
  "x := 0;",
  "y := 1;",
  "i := 0;",
  "while (i < count) do",
  "  print x;",
  "  tmp := x + y;",
  "  x := y;",
  "  y := tmp;",
  "  i := i + 1",
  "end  "
  ]
