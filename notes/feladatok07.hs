
import Control.Monad


-- Reader monád
------------------------------------------------------------

-- Reader monád: pontosan ugyanaz mint (->)
newtype Reader a b = Reader {runReader :: a -> b}

instance Functor (Reader a) where
  fmap f (Reader g) = Reader (f . g) -- függvény Functor instance-ja

instance Applicative (Reader a) where
  pure  = return
  (<*>) = ap     -- importálva Control.Monad-ból

instance Monad (Reader a) where
  return x = Reader (const x)
  Reader f >>= g = Reader $ \a -> runReader (g (f a)) a

-- Mire jó?
--   van egy read-only konfiguráció/adat
--   abban különbözik State-től, hogy nem módosítható az állapot, csak
--   olvasható.
-- Ugyanúgy használható, mint a State monád, de nincs módosítás

-- Két alapművelet:

-- get megfelelője
ask :: Reader r r
ask = Reader id    -- lekérdezzük az "állapotot"

-- lokálisan scope-olt állapotváltoztatás
local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader $ \a -> g (f a) -- függvénykompozíció

-- példák:

foo :: Reader Int Int
foo = do
  x <- ask
  local (+x) $ do
    x <- ask
    pure x

-- State helyett gyakran elég Reader monád.
-- (amikor elég lokálisan változtatni)

-- feladat: eval
--          lookup :: Eq k => k -> [(k, v)] -> Maybe v
data Exp =
    IntLit Int          -- literál
  | Add Exp Exp         -- +
  | Var String          -- változó
  | Let String Exp Exp  -- let x = e1 in e2


eval :: Exp -> Reader [(String, Int)] Int
eval (IntLit n)  = pure n
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2

  -- monádikusan
  -- do
  --   x <- eval e1
  --   y <- eval e2
  --   pure (x + y)

eval (Var x) = do
  env <- ask
  case lookup x env of
    Nothing -> undefined
    Just v  -> pure v
eval (Let x e1 e2) = do   -- (let x = e1 in e2)
  v <- eval e1
  -- adjuk hozzá lokálisan az értéket a környezethez:
  local (\env -> (x, v):env) (eval e2)

-- Általában: olyan nyelvet interpretálunk, típusellenőrzünk, fordítunk stb.
-- aminek lexikális a scope-ja (egymásba ágyazott statikusan ismert scope-ok)
-- akkor Reader-t kell használni (majdnem mindig)
