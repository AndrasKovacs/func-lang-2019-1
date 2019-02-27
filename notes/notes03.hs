{-# language KindSignatures, GADTs, EmptyCase, RankNTypes, UnicodeSyntax #-}

-- Curry-Howard megfeleltetés, polimorf függvények
--------------------------------------------------------------------------------

-- Állítás: (totális) funkcionális programok írása, megfeletethető logikai
--          bizonyítások írásának

-- (fogalom: Curry-Howard megfeleltetés)

-- nulladrendű logika:
--   állítások, implikáció, logikai és, logikai vagy

-- Haskell-ben:
-- állítások: típusváltozókat felveszünk polimorf függvénydefinícióban

-- implikáció: (->)

-- bármilyen állítás következménye önmagának:
id' :: a -> a
id' = \x -> x
    -- logika: mindig van ismert állításoknak egy listája
    -- programozás: lokális scope, változónévvel hivatkozunk
    --              ismert dolgokra

    -- lambda behozza a scope-ba a hipotetikus állítás
    -- bizonyítását

modusPonens :: (a -> b) -> a -> b
modusPonens = \f a -> f a


-- Logikai és: párképzés

lemma1 :: ((a, b) -> c) -> (a -> b -> c) -- curry
lemma1 f a b = f (a, b)

lemma2 :: (a -> b -> c) -> ((a, b) -> c)
lemma2 f (a, b) = f a b

-- Logikai ekvivalencia
type LogEqv a b = (a -> b, b -> a)

lemma3 :: LogEqv ((a, b) -> c) (a -> b -> c)
lemma3 = (lemma1, lemma2)

-- Logikai vagy: Either a b
lemma4 :: LogEqv (Either a b -> c) (a -> c, b -> c)
lemma4 = (\f -> (f . Left, f . Right),
          \(f, g) x -> case x of Left a -> f a; Right b -> g b)

-- Logikai igaz: unit típus (egy értékű típus)
-- standard definíció: ()
-- típus neve: ()
-- egy értéke van, aminek a neve: ()
-- (intuíció: üres tuple)

type True = ()

lemma5 :: a -> True
lemma5 = \_ -> ()

-- Logikai hamis (ellentmondás): üres típus
data False

-- Haskell logikailag inkonzisztens, mert írhatunk loop-ot Ezért fontos, hogy
-- csak totális programok értelmezhetők helyes bizonyításként.
falsehood :: False
falsehood = falsehood

exFalso :: False -> a
exFalso x = case x of

lemma6 :: False -> False
lemma6 x = x

-- Logikai negáció:
-- (pontosan akkor tudunk "a -> False" függvényt definiálni,
--  ha nem tudunk megadni "a" értéket)
type Not a = a -> False

trivial :: Not False    -- a False negációja logikailag ekvivalens True-val
trivial = \x -> x

lemma7 :: Not (Either a (Not a)) -> Not a
lemma7 f = \a -> f (Left a)

-- nem implementálható függvényként
-- doubleNegElim :: ((a -> False) -> False) -> a
-- doubleNegElim f = _

-- Programokkal és típusokkal konstruktív logikát tudunk leírni.


-- Elsőrendű logika
--------------------------------------------------------------------------------

-- higher-rank polymorphism

map' :: ∀ a b. (a -> b) -> [a] -> [b]
map' = undefined

id'' :: ∀ a. a -> a
id'' x = x

myFun :: (∀ a. a -> a) -> (Bool, [Bool])
myFun g = (g True, g [])

-- példa higher-rank polimorfizmus gyakorlati használatára: ST monad.
-- (lásd: Control.Monad.ST)

-- elsőrendű állítás
fo1 :: ∀ (p :: * -> *) (q :: * -> *).
      (∀ a. (p a, q a)) -> ∀ a. q a
fo1 (pa, qa) = qa

-- magasabbrendű állítás
higherOrder :: ∀ (p :: (* -> *) -> *) (q :: * -> *). p q -> p q
higherOrder x = x


-- Létezés bizonyítása: Haskell-ben nehézkes.
data Exists p where
  Something :: ∀ p x. p x -> Exists p

stupidLemma :: Exists []
stupidLemma = Something [True]  -- p legyen [], x legyen Bool
                                -- megadtam egy [Bool]-t
                                -- ebből tudok csinálni (Exists [])
