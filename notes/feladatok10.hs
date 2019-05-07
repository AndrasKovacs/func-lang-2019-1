{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor,
    KindSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

import Control.Monad

-- Monad transformers
----------------------------------------

-- motiváció: egyszerre több hatást használni, több monádból

-- egy megoldás: írunk egy új monádot, amiben minden benne van
-- Pl. korábbi Parser: egyszerre State és Maybe

-- nem akarjuk a custom monádokat mindig újraírni, akkor
-- monád transzformereket érdemes használni

-- Működés: ahelyett, hogy (m :: * -> *)-et használunk,
-- minden monád transzformer kap egy extra (m :: * -> *)
-- típusparamétert.

-- State transformer
--    State {runState :: s -> (a, s)}
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}
--  deriving Functor

instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \s -> fmap (\(a, s) -> (f a, s)) (g s)

instance Monad m => Applicative (StateT s m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  StateT sa >>= f = StateT $ \s -> do
    (a, s') <- sa s
    runStateT (f a) s'

-- -- példa monomorf lift implementációra
-- lift :: Monad m => m a -> StateT s m a
-- lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma

-- get :: Applicative m => StateT s m s
-- get = StateT $ \s -> pure (s, s)

-- put :: Applicative m => s -> StateT s m ()
-- put s = StateT $ \_ -> pure ((), s)

-- foo :: StateT Int IO ()
-- foo = do
--   lift $ putStrLn "hello"
--   n <- get
--   put (n + 2000)

-- main :: IO ()
-- main = fst <$> runStateT foo 100


-- get :: Applicative m => StateT s m s
-- get = StateT $ \s -> pure (s, s)

-- put :: Applicative m => s -> StateT s m ()
-- put s = StateT $ \_ -> pure ((), s)


-- osztály monád transzformerekre
class MonadTrans (t :: (* -> *) -> * -> *) where -- (KindSignatures szüks.)
  lift :: Monad m => m a -> t m a

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma

-- probléma: get-et és put-ot milyen típuson használjuk?

newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}
  deriving Functor

instance Monad m => Applicative (ExceptT e m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ExceptT e m) where
  return a = ExceptT (return (Right a))
  ExceptT mea >>= f = ExceptT $ do
    ea <- mea
    case ea of
      Left e  -> return (Left e)
      Right a -> runExceptT (f a)

-- throwError :: Monad m => e -> ExceptT e m a
-- throwError e = ExceptT (return (Left e))

instance MonadTrans (ExceptT e) where
  lift ma = ExceptT (Right <$> ma)

-- f1 :: ExceptT String (StateT Int IO) ()
-- f1 = do
--   lift $ lift $ putStrLn "hello"
--   n <- lift get
--   lift $ put $ n + 100
--   throwError "error"

-- konkrétan erre transzformer stack-ra: hiba esetén az állapot
-- nem veszik el

-- runStateT (runExceptT f1) 100 :: IO (Either String (), Int)
-- > hello
-- > (Left "error",200)

-- hiba esetén eldobjuk az állapotot (ilyen volt a Parser monád)
f1' :: StateT Int (ExceptT String IO) ()
f1' = undefined

-- Int -> ExceptT String IO ((), Int)
-- Int -> IO (Either String ((), Int))

-- Mindig n darab lift kell belső hatásokra
-- Ehelyett: mtl library-t, ahol nem kell lift sehol, kivéve


-- mtl style MonadTrans működése
------------------------------------------------------------

-- legyen egy osztály, aminek az instance-jai olyan
-- monád transzformer stack-ek, amiben valahol van pontosan
-- egy StateT

-- FunctionalDependencies-el megszorítjuk a MonadState-et:
-- Egy "m"-hez nem tartozhat két különböző "s"
-- tehát:
class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance Monad m => MonadState s (StateT s m) where
  get   = StateT $ \s -> pure (s, s)
  put s = StateT $ \_ -> pure ((), s)

-- -- alternatív megoldás TypeFamily-ve
-- class MonadState m where
--   type StateOf m
--   get :: m (StateOf m)
--   put :: StateOf m -> m ()

-- instance Monad m => MonadState (StateT s m) where
--   type StateOf (StateT s m) = s
--   get   = StateT $ \s -> pure (s, s)
--   put s = StateT $ \_ -> pure ((), s)

instance (MonadState s m) => MonadState s (ExceptT e m) where
  get   = lift get
  put s = lift (put s)

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

instance MonadIO IO where
  liftIO = id

f1 :: ExceptT String (StateT Int IO) ()
f1 = do
  liftIO $ putStrLn "hello"
  n <- get
  put $ n + 100
  pure ()

-- Hogy használjuk mtl-style transzformereket?
------------------------------------------------------------

-- két verzió:
--   1. konkrét stack, erre megadnak egy type synonym-t, és
--      sok dolgot ebben a konkrét stack-ben írnak

data MyError
data MyState
type MyMonad = StateT MyState (ExceptT MyError IO)

myFun1 :: MyMonad ()
myFun1 = undefined

-- másik verzió:
--   olyan függvényeket írunk, amik polimorfak a használt hatásokban, és
--   aztán a típusuk specializálódik egy konkrét stack-re, amikor
--   futtatjuk vagy komponáljuk őket

myPop :: MonadState [a] m => m (Maybe a)
myPop = do
  as <- get
  case as of
    []   -> pure Nothing
    a:as -> put as >> pure (Just a)

-- n = 100
