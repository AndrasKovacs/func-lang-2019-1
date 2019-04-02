{-# language DeriveFunctor #-}

import Control.Monad

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State sa >>= f = State $ \s -> case sa s of (a, s) -> runState (f a) s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f =
  get >>= \s ->
  put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

{-
Implementáljuk a következő függvényeket, de mindegyiket kétszer, `State`
monáddal, és anélkül is. A `State` nélküli implementációban csak adjuk mindig
tovább az állapotot paraméterként.

State monádos implementációban ne használjuk a State konstruktort!
-}

type State' s a = s -> (a, s)

pop :: State [a] (Maybe a)
pop = undefined

pop' :: State' [a] (Maybe a)
pop' = undefined

push :: a -> State [a] ()
push = undefined

push' :: a -> State' [a] ()
push' = undefined


-- Implementáljuk a következő függvényt State monád nélkül.
-- Minden elemre hajtsuk végre a függvény által adott műveletet,
-- az összes eredményt adjuk vissza listában.
mapState' :: (a -> s -> (b, s)) -> [a] -> s -> ([b], s)
mapState' f []     s = ([], s)
mapState' f (a:as) s = case f a s of
  (b, s') -> case mapState' f as s' of
    (bs, s'') -> (b:bs, s'')

-- pl: mapState' (\x s -> (x, x + s)) [0..10] 0
--   == ([0,1,2,3,4,5,6,7,8,9,10],55)

-- Legyen mapM' a fenti függvény általánosítása tetszőleges monádra.
-- (a "mapM" standard függvény ugyanezzel a típussal).
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f []     = pure []
mapM' f (a:as) =
  f a >>= \b ->
  mapM' f as >>= \bs ->
  pure (b:bs)

-- példa: runState (mapM (\x -> State $ \s -> (x, s + x)) [0..10]) 0
--        runState (mapM (\x -> x <$ modify (+x)) [0..10]) 0

-- do notáció: szintaktikus cukor
mapM'' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'' f []     = pure []
mapM'' f (a:as) = do
  b  <- f a
  bs <- mapM' f as
  pure (b:bs)

-- do notáció:

-- ma >>= \a -> x

-- do
--   a <- ma
--   x

-- ma >> mb

-- do
--  ma
--  mb

-- egy soros do blokk nem csinál semmit
-- több soros, bind-nak a szintaktikus cukorkája

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' f []     = pure []
filterM' f (a:as) = do
  b <- f a
  if b then (a:) <$> filterM' f as
       else filterM' f as

-- replicateM' :: Int -> m a -> m [a]
-- replicateM' = _

-- (Északi épület 7.15 terem: Parser monád Haladó Haskell, 17:45-től)
