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
mapState' :: (a -> State' s b) -> [a] -> State' s [b]
mapState' = undefined


-- Legyen mapM' a fenti függvény általánosítása tetszőleges monádra.
-- (a "mapM" standard függvény ugyanezzel a típussal).
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' = undefined
