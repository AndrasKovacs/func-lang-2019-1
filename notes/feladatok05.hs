{-# language DeriveFunctor #-}

import Prelude hiding (Maybe(..))
import Control.Monad

-- Foldable instance-ok
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
data RTree a = RNode a [RTree a] deriving Show

instance Foldable Tree where
  foldr f z (Leaf a)   = f a z
  foldr f z (Node l r) = foldr f (foldr f z r) l

-- tanulság: rekurzív típus foldr-nél, mindig a jobb oldali
-- rész-struktúrát kell először foldr-ezni, és az eredményt
-- használni a bal részstruktúra foldr-nél a z-kezdőértéknek.

instance Foldable RTree where
  foldr f z (RNode a ts) =
    f a (foldr (\t b -> foldr f b t) z ts)

  -- foldr f z (RNode a []) == f a z

  -- foldr f z (RNode a [RNode b []])
  --   == f a (foldr f z (RNode b []))
  --   == f a (f b (foldr f z []))
  --   == f a (f b z)

------------------------------------------------------------

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
-}

-- return :: a -> m a
-- mellékhatás nélkül visszaadja az "a" értéket

-- ma >>= \a -> mb
-- egymás után végrehajtunk két mellékhatásos műveletet,
-- ahol a második hatás függhet az első visszatérési értékétől

-- >> :: m a -> m b -> m b
-- egymás után végrehajtunk két műveletet, a második művelet
-- nem függ az első (visszatérési) értékétől

-- két dolog:
--   - minden művelethez van egy típus, ami a visszatérési
--     értékek típusa
--      (ma :: m a), akkor "a" típusú értéket adhat vissza
--   - lehet minden műveletnek mellékhatása, amit a "Monad m"
--     instance definiál.

-- Példa: Maybe
-- (Csak Monad instance-okat definiálunk, az Applicative
--  instance-ot csak definiáljuk a Monad instance segítségével)

data Maybe a = Nothing | Just a deriving (Eq, Show, Functor)

instance Applicative Maybe where
  pure  = return
  (<*>) = ap

  -- ap :: m (a -> b) -> m a -> m b
  -- ap mf ma = mf >>= \f -> ma >>= \a -> return (f a)

-- Hibázhatunk Nothing-al, viselkedjen kb. kivétel dobásként
instance Monad Maybe where
  return a = Just a   -- return :: a -> Maybe a
  Nothing >>= f = Nothing
  Just a  >>= f = f a

-- >> operátor illetve >>= is balra zárójelez,
-- leggyengébb precedenciálval

-- >> :: Monad m => m a -> m b -> m b
-- >> ma mb = ma >>= \_ -> mb

foo :: Maybe Int
foo = Nothing >> Just 10

bar :: Maybe Int
bar = Just 100 >> Just 100 >> Just 200


-- State
------------------------------------------------------------

-- egy adott "s" típusú írható/olvasható érték
-- ennek írása-olvasása a mellékhatás

-- előző órai feladatban segédfüggvény
-- label' :: Int -> (Tree (a, Int), Int)


newtype State s a = State (s -> (a, s))

instance Functor (State s) where
  fmap f (State st) = State $ \s -> let (a, s') = st s in (f a, s')

-- lefuttatja a műveletet, csak a visszatérési értékre
-- alkalmazza a függvényt

-- példa: IO monádban fmap
-- fmap length getLine   (művelet lefut, aztán végeredményre
--                        alkalmazunk függvényt)

-- fmap operátoros infix szinonímája: <$>
-- length <$> getLine

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))  -- s változatlan, érték a

  -- először a bal művelet, aztán a jobb művelet végrehajtása
  State st >>= f = State $ \s ->
    case st s of              -- első művelet
      (a, s') -> case f a of  -- hívjuk meg a függvényt
                              -- ami megadja a 2. műveletet
        State st' -> st' s'   -- második művelet

-- (State s a) használata  (standard függvények)
runState :: State s a -> s -> (a, s)
runState (State f) = f

-- csak a végeredményt adja vissza, az állapotot nem
evalState :: State s a -> s -> a
evalState sta s = fst (runState sta s)

-- csak a végső állapotot adja vissza
execState :: State s a -> s -> s
execState sta s = snd (runState sta s)

-- két alapvető művelet: get, put

-- get: jelenlegi állapot olvasása
get :: State s s
get = State $ \s -> (s, s)  -- pár első eleme a visszatérési érték
                            -- ami itt a jelenlegi állapot

-- put: jelenlegi állapot felülírása
put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- példa listaműveletekkel és Int-ekkel

-- megnöveli az Int típusú állapotot 1-el
increment :: State Int ()
increment = get >>= \n -> put (n + 1)

-- hajtsuk végre kétszer
incrementTwice = increment >> increment

--
num = execState incrementTwice 0


push :: a -> State [a] ()
push a = get >>= \as -> put (a:as)

-- biztonságos pop művelet
pop :: State [a] (Maybe a)
pop = get >>= \as -> case as of
        []   -> return Nothing
        a:as -> put as >> return (Just a)


myAction :: State [Int] (Maybe Int)
myAction = push 0 >> push 1 >> push 10 >> pop

-- 1000-szer pop-ol
myAction2 :: State [Int] ()
myAction2 = replicateM_ 1000 pop

num2 = evalState myAction []
num3 = execState myAction2 (replicate 1100 10)

------------------------------------------------------------
