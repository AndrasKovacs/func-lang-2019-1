
-- ADT-k
--------------------------------------------------------------------------------

{-

Ajánlott olvasmány:

https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types

ADT: algebraic data type. Miért algebrai? Azért, mert algebrai műveletekkel
(összeadás, szorzás, hatványozás) ki lehet számolni egy típus lehetséges
értékeinek a számát.

-}

-- Az egy elemű típus, algebrailag az 1 konstans:
data One = One deriving Show
  -- típus neve: One
  -- érték konstruktor neve: One

-- standard megfelelő: (), értéke szintén ()
-- neve: "unit" típus.

one :: One
one = One

unit :: ()
unit = ()


-- (megjegyzés: Haskell-ben két értéke is létezik One-nak, a másik érték a végtelen loop.
--  egy parciális függvényeket támogató nyelvben már nem teljesen igazak az egyszerű algebrai
--  képletek az elemszámok kiszámolásáre, de most eltekintünk a kopmlikációktól.)
one2 :: One
one2 = one2   -- rekurzív definíció, végtelen loop


-- 0 elemű típus
data Zero   -- nincs konstruktor


-- A két elemű típus a Bool
type Two = Bool

-- type: csak típusszinoníma

-- newtype: egy konstruktoros, és egy mezős data definíció
-- futásidőben nincs jelen a newtype, csak fordítási időben


two :: Two
two = False


-- Típusok összeadása:
-- standard definíció:
-- data Either a b = Left a | Right b

-- példa: Either Bool Bool

ebb1 :: Either Bool Bool
ebb1 = Left True

ebb2 :: Either Bool Bool
ebb2 = Right False

-- 2 + 2 = 4 lehetséges érték van Either Bool Bool-ban

-- Típusok szorzása:
data Pair a b = Pair a b deriving (Eq, Show)
-- standard definíciója: (a, b) jelöli a típust és az értéket egyaránt.

-- halmazelméleti megfelelők:
--   (a, b): megfelel (A × B) rendezett párok halmazának.
--   Either a b: megfelel "a" és "b" diszjunkt uniójának.


-- Típusok hatványozása:
type Exp a b = b -> a
-- a^b hatvány megfelel b -> a függvénynek.

-- pl: R^n  valós számok n-tuple-je matematikában, és n-tuple-k
--     reprezentálhatók függvényekként egy n-elemű típusból.

-- pl: (Bool -> Int) ~ (Int, Int), mivel True-ra és False-ra is kapunk
--     egy Int-et.

-- pl: Int^2 ~ (Int × Int)

-- pl: hatványhalmaz matematikában: részhalmazok halmaza
--     Minden (f :: a -> Bool) függvény reprezentál egy részhalmazt,
--     karakterisztikus függvényként, azaz (f x == True) ha x eleme a
--     részhalmaznak.


-- Aritmetikai azonosságok mentén lehet refaktorálni típusokat.
-- Egyszerű példa: Either Int Zero ~ Int, azaz 2^64 + 0 = 2^64


-- Rekurzív adattípusok
--------------------------------------------------------------------------------

-- természetes számok típusa
data Nat = Zero | Suc Nat deriving Show

-- Megfelelő algebrai egyenlet: Nat = 1 + Nat Ha "Nat" egy halmaz jelöl, az "="
-- pedig halmazok bijekcióját (izomorfizmus), akkor a fenti egyenlet megoldása
-- valóban a természetes számok halmaza.

n0 :: Nat
n0 = Zero

n3 :: Nat
n3 = Suc (Suc (Suc Zero))


-- Adattípus, ami Haskell-ben definiálható és nemüres, halmazelméletben viszont
-- csak üres megoldása van.
data Weird = Weird (Weird -> Bool)

weird1 :: Weird
weird1 = Weird $ \_ -> True

weird2 :: Weird
weird2 = Weird $ \(Weird f) -> f weird1


-- halmazelmélet: Cantor tétel: ha A nemüres, 2^A és A között nincs bijekció.
-- A Haskell típusok különböznek a halmazelméleti halmazoktól, ezért van nemüres
-- megoldás is.

-- Hogy pontosan miben különböznek, azt a klasszikus denotációs szemantika tárgyalja.
-- https://en.wikipedia.org/wiki/Denotational_semantics


-- Egyéb standard típusok
------------------------------------------------------------

-- standard lista újradefiniálása
data List a = Nil | Cons a (List a) deriving (Show)

-- bináris, leveles fák típusa
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

t1 :: Tree Int
t1 = Leaf 1000

t2 :: Tree Int
t2 = Node (Leaf 0) (Node (Leaf 1) (Leaf 2))

-- listát csináljunk fából
-- (két konstruktor --> két mintára illeszthetünk)
treeToList :: Tree a -> [a]
treeToList (Leaf a)   = [a]
treeToList (Node l r) = treeToList l ++ treeToList r

-- (probléma: a fenti definíció kvadratikus worst-case, mivel
--  xs ++ ys költsége xs hosszával arányos.)


-- Típusosztályok (typeclass)
--------------------------------------------------------------------------------

-- A típusosztályok lényegében egy mód automatikus kódgenerálásra, ami eléggé
-- megszorított ahhoz, hogy robusztus legyen, viszont elég liberális ahhoz, hogy
-- sok gyakori esetben tudjunk unalmas kódot automatikusan generálni.

-- Példa unalmas kódra:

-- listák egyenlősége
listEq :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listEq f []     []     = True
listEq f (x:xs) (y:ys) = f x y && listEq f xs ys
listEq f _      _      = False

-- Bool egyenlősége
boolEq :: Bool -> Bool -> Bool
boolEq True True = True
boolEq False False = True
boolEq _ _ = False


-- Ha bármilyen összetett típusunk van, akkor kézzel kell összekombinálni
-- az egyes típusok eq implementációit:
listListListBoolEq :: [[[Bool]]] -> [[[Bool]]] -> Bool
listListListBoolEq = listEq (listEq (listEq boolEq))

-- Végtelen sok különböző összetett típuskifejezés lehetséges, és nem szeretnénk
-- mindegyikre kézzel összeragasztani ez egyenlőséget vizsgáló függvényt. Megadunk
-- ezért egy typeclass-t:

class Eq' a where           -- class declaration
  eq :: a -> a -> Bool      -- class method (lehet egy vagy több metódus)

instance Eq' Bool where     -- instance
  eq = boolEq

-- megszorított instance: listára csak akkor van egyenlőségviszgálat, ha
-- az elemekre is van.
instance Eq' a => Eq' [a] where
  eq = listEq eq

listListListBoolEq' :: [[[Bool]]] -> [[[Bool]]] -> Bool
listListListBoolEq' = eq -- ezt GHC lényegében a korábbi
                         -- listEq (listEq (listEq boolEq)) definícióra
                         -- fordítja le.

-- (Eq': standard definíciója: Eq, művelet neve pedig (==))
-- példa: [1, 2, 3] == [5, 6, 7]


-- megszorítások (constraint) függvényekben:
myFun :: Eq a => a -> a -> b -> b -> b
myFun a1 a2 b1 b2 = if a1 == a2 then b1 else b2

-- myFun csak olyan a-ra alkalmazható, amire van (Eq a) instance.

-- lehet több megszorítás is, ekkor zárójelbe kell tenni őket => előtt.
myFun2 :: (Eq a, Eq b) => a -> a -> b -> b -> Bool
myFun2 a1 a2 b1 b2 = a1 == a2 && b1 == b2


-- További standard egyszerű osztályok: Ord, Show, Read (nézzünk utána ezeknek)

-- Semigroup és Monoid osztályok.
-- Mindkettő standard.

-- Olvasmány:
--  http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids

{-

-- félcsoport: a <> művelet konvenció szerint asszociatív:
class Semigroup a where
  (<>) :: a -> a -> a

-- példa, lista félcsoport
instance Semigroup [a] where
  (xs <> ys) = xs ++ ys

-- monoid: egységelemes félcsoport. A "Semigroup a =>" egy superclass megszorítás,
-- ezt azt jelenti, hogy csak akkor definiálható "Monoid a" instance, ha van
-- "Semigroup a" instance. Kovenciá szerint, a "mempty" a "<>" művelet egységeleme
-- kell, hogy legyen.

class Semigroup a => Monoid a where
  mempty :: a

-- példa, lista monoid
instance Monoid [a] where
  mempty = []

-}

-- példa: Monoid instance olyan függvényekre, amelyek visszatérési típusa Monoid:
class Semigroup' a where
  combine :: a -> a -> a

class Semigroup' a => Monoid' a where
  mempty' :: a

instance Semigroup' b => Semigroup' (a -> b) where
  combine f g = \a -> combine (f a) (g a)

instance Monoid' b => Monoid' (a -> b) where
  mempty' = const mempty'
