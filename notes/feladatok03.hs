
-- téma: Functor, Foldable


-- Functor
--------------------------------------------------------------------------------

-- standard osztály

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- pl. Functor [],  fmap :: (a -> b) -> [a] -> [b]
--            (ahol az "f" ebben az esetben a [])

-- (írhatjuk a lista típust is prefix formában)

list1 :: [] Int
list1 = [0..10]


-- Functor *nem* típusok osztálya, hanem típuskonstruktorok osztálya. Ha
-- "Functor f", akkor "f" egy egy paraméteres típuskonstruktor.


-- Típuskonstruktorok vs. típusok (típusok típusai)
--------------------------------------------------------------------------------

-- típusok típusai

-- data MyData a b c d e f g h = Con2 a b c d e f

-- típusszintű kifejezéseknek is van típusrendszere
-- (megkülönböztetjük az "n" és "m" paraméteres típusokat)

-- pl: ghci-ben :k (azt rövidíti, hogy "kind")
-- > :k Int
-- > Int :: *
-- > :k []
-- > [] :: * -> *

-- Either :: * -> * -> *
-- Either Int :: * -> *
-- Either Int Int :: *

-- "*" : A paraméter nélküli szimpla típusok típusa
-- "* -> *" : Az olyan típusok típusa, amelyeknek egy db. "*"
--            paramétere van.

-- ((T :: (* -> *) -> *)  is lehetséges)


-- Functor folytatás
--------------------------------------------------------------------------------

-- instance Functor [] where
--   fmap :: (a -> b) -> [a] -> [b]
--   fmap = map

-- homogén párok: mindkét mező típusa ugyanaz
data HomPair a = HomPair a a

instance Functor HomPair where
  fmap f (HomPair x y) = HomPair (f x) (f y)

-- szimplán pár típusra is adhatok Functor instance-t

data Pair a b = Pair a b   -- (,)

-- Lerögzítjük az első mező típusát.
-- Ekkor kapunk olyan típuskifejezést, ami egy paraméteres
-- És így az fmap a második mező fölött map-el.
instance Functor (Pair c) where
  fmap f (Pair c a) = Pair c (f a)

-- Ha két paraméter fölött szeretnénk mappelni:
-- class Bifunctor f where
--   bimap :: (a -> a') -> (b -> b') -> f a b -> f a' b'

-- Feladat 1: adjuk meg fmap-et:
-- Segítség: fmap :: (a -> b) -> (c -> a) -> (c -> b)

-- instance Functor ((->) c) where
--   fmap = (.)


-- Type hole-ok használata
--------------------------------------------------------------------------------

-- underscore-t írunk egy kifejezésbe: ghc a lyuk típusát és a lokális
--   definíciók típusát megjeleníti.

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' ((->) c) where
  fmap' f g x = f (g x)

-- példa: type hole használatára:
f :: (Either a b -> c) -> ((a -> c), (b -> c))
-- f = (_ :: (Either a b -> c) -> ((a -> c), (b -> c)))
-- f = \g -> (_ :: (a -> c, b -> c))
-- f = \g -> ((_ :: a -> c), (_ :: b -> c))
-- f = \g -> ((\a -> (_ :: c)), (\b -> (_ :: c)))
-- f = \g -> ((\a -> g (_ :: Either a b)), (\b -> g (_ :: Either a b)))
f = \g -> ((\a -> g (Left a)), (\b -> g (Right b)))


----------------------------------------

newtype Compose f g a = Compose (f (g a))

-- :k Compose
-- Compose :: (* -> *) -> (* -> *) -> * -> *

-- példák
comp1 :: Compose [] [] Int
comp1 = Compose [[100, 200], [100]]

comp2 :: Compose [] ((->) Int) Bool
comp2 = Compose [\_ -> True]

-- Feladat 2: hole-ok használatával írjuk
-- meg a következő instance-t
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose (nestedMap f fga)

nestedMap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
nestedMap f fga = fmap (fmap f) fga


-- Foldable osztály (standard)
------------------------------------------------------------

-- fold-olható struktúrák osztálya
-- másképpen: listává alakítható struktúrák osztálya

-- class Foldable f where
--   foldr :: (a -> b -> b) -> b -> f a -> b

-- (lényegében az is elég, ha csak egy "toList :: f a -> [a]" függvényt
--  adunk meg)

-- (példa: Functor, de nem Foldable: függvény
--  Haskell-ben lényegében csak a függvény az ilyen típus)

-- Feladat: írjunk "Foldable Tree" típust
-- úgy, hogy foldr definícióját adjuk meg
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

-- példa
instance Foldable Tree where
  foldr = undefined
