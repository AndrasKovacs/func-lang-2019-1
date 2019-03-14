{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Control.Monad.State


-- Monádok
------------------------------------------------------------


-- 1. feladat

data Tree a = Node a [Tree a]
  deriving (Show, Functor, Foldable, Traversable)

-- adjunk vissza egy fát, amiben az elemek a balról jobbra
-- bejárás sorrendjében Int-el címkézve vannak.
-- (Node-ban az "a" elemet úgy értelmezzük, hogy balra van az
--  összes részfában levő elemtől)
-- label :: Tree a -> Tree (a, Int)
-- label t = undefined

  -- evalState
  --   (traverse (\a -> (,) a <$> (get <* modify (+1))) t) 0


-- State monad megoldás label-re
------------------------------------------------------------


-- State monád: lokális írható-olvasható állapotot használni
--  imperatív megoldás: egy írható változó + bejárás, minden
--    elemnél (+1) a változóhoz

--  Ha van érték, aminek a típusa "State s a": olyan állapotot módosító
--  program, aminek a futtatásához meg kell adni egy "s" típusú kezdőértéket
--  és a végeredményként kapunk (a, s) párt, ahol az "s" a legutolsó értéke
--  az állapotnak.

label :: Tree a -> Tree (a, Int)
label t =
  evalState
    (traverse (\a -> do {i <- get; put (i + 1); pure (a, i)}) t) 0


-- ez lényegében a traverse függvény, specializálva a Maybe típusra
-- mappelünk a fa fölött, viszont az a mellékhatás, hogy Nothing-al
-- hibázhat minden elem feldolgozása
mapMaybe :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybe = traverse


-- Monad class definíció, működés
------------------------------------------------------------

-- (superclass az Applicative, de most nem foglalkozunk vele)

-- class Functor f => Applicative f where
--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b

-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b


-- megjegyzések:
--   ">>=" kiejtése: bind
--   "<*>" kiejtése: ap
--   "return" implementációja mindig ugyanaz kell, hogy legyen, mint a pure
--     (historikus hülyeség, hogy ugyanaz a függvény két néven két osztályban
--      szerepel)
--   best practice: mindig a pure-t használjuk


-- Monád motiváció
------------------------------------------------------------

-- szeretnénk mellékhatásos programot írni, úgy hogy:
--   minden mellékhatásos program típusából látszik, hogy milyen
--   mellékhatást használ. Nagy előny: tudom, hogy nincsen mellékhatás,
--   ha nincsen jelölve a típusban.

-- imperatív interface-t és szintaxist adunk olyan programoknak,
-- amit a háttérben (általában) mellékhatások nélküli függvények
-- implementálnak (kivétel: IO, exception, concurrency, etc.)


-- pure értelmezése (TODO)
-- >>= értelmezése: imperatív szekvenciálás implementálása
--                  gyakran pontosevssző C-szerű nyelvekben
--                  statement1 ; statement2


-- Tiszta Haskell-ben nincs kiértékelési sorrend megkötve/megadva

--       f x y         -- nem tudom, hogy x vagy y értékelődik ki először

--       let x = t
--           y = u
--       in v

-- viszont mellékhatásokat szeretnék konkrét, kontrollált sorrendben
-- végrehajtani


--      első művelet    második műveletet
-- >>= ::   m a    ->      (a -> m b)    ->     m b

-- bind végrehajt *egymás után* két mellékhatásos műveletet,
-- ahol a második művelet hatása függhet az első művelet visszatérési
-- értékétől

-- (szintén standard)
-- >> :: Monad m => m a -> m b -> m b
-- (egymás után végrehajta a két műveletet, visszaadjuk a második művelet
--  eredményét)


-- példa >>=-re, és >>-re C-ben

-- void foo(){
--   int x = exp1;
--   exp2
-- }

-- exp1 >>= (\x -> exp2)

-- void foo (){
--   statement1;
--   statement2
-- }

-- statement1 >> statement2

------------------------------------------------------------
