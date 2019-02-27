{-# language TypeOperators, UnicodeSyntax #-}

-- Típusosztályok:
--------------------------------------------------------------------------------

-- Kódgenerálás típusok struktúrája szerint

class Eq' a where
  eq :: a -> a -> Bool  -- method

instance Eq' Bool where
  -- eq :: Bool -> Bool -> bool
  eq True  True  = True
  eq False False = True
  eq _     _     = False

notEq :: Eq' a => a -> a -> Bool
notEq x y = not (eq x y)

instance Eq' a => Eq' [a] where
  -- pontonkénti egyenlőség a listák egyenlősége
  eq []     []     = True
  eq (x:xs) (y:ys) = eq x y && eq xs ys
  eq _      _      = False

f1 :: [[[Bool]]] -> [[[Bool]]] -> Bool
f1 = eq

instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq (x, y) (x', y') = eq x x' && eq y y'

f2 :: ([Bool], [[Bool]]) -> ([Bool], [[Bool]]) -> Bool
f2 = eq

-- Rust, Swift : szintén type class alapú absztrakció


--------------------------------------------------------------------------------

-- félcsoport (asszociatív bináris művelet, standard)
-- class Semigroup a where
--   (<>) :: a -> a -> a

-- -- egységelemes félcsoport (szintén standard)
-- class Semigroup a => Monoid a where
--   mempty :: a

-- instance Semigroup [a] where
--   xs <> ys = xs ++ ys -- listák összefűzés

-- instance Monoid [a] where
--   mempty = []


-- programok szervezését inspirálja: absztrakt algebra, kategóriaelmélet
--------------------------------------------------------------------------------


-- standard, nincs köze C++ funktorokhoz:
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--   -- fmap :: (a -> b) -> [a] -> [b]
--   fmap f []     = []
--   fmap f (x:xs) = f x : fmap f xs

-- Functor: olyan f típuskonstruktorok osztálya, amire
-- van "map" jellegű függvényalkalmazás

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Stream a = Cons a (Stream a) -- végtelen listák típusa

ones :: Stream Int
ones = Cons 1 ones  -- Cons 1 (Cons 1 (Cons 1 ......

takeStream :: Int -> Stream a -> [a]
takeStream 0 (Cons x xs) = []
takeStream n (Cons x xs) = x : takeStream (n - 1) xs

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


-- instance Functor ((->)
-- függvénytípus: ->, két paraméteres típus
--

fun :: (->) Int Int   -- Int -> Int
fun x = x + x

-- instance Functor ((->) a) where
--   fmap f g = f . g

fmapFun :: (a -> b) -> (c -> a) -> c -> b
fmapFun f g = f . g


-- Variancia kérdése: Functor kovariáns, de van kontravariáns verzió is,
-- "Contravariant" néven:
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

data Contra a = Contra (a -> Bool) (a -> Int)

instance Contravariant Contra where
  contramap f (Contra g h) = Contra (g . f) (h . f)

-- létezik-e olyan típus "f", amire igaz Functor f, és az is igaz, hogy
-- Contravariant f? Létezik, de az összes ilyen típus konstans funktor, azaz
-- olyan típuskonstruktor, ami nem használja föl a típusparamétert sehol.

-- konstans funktor: nem függ a paramétertől
data ConstInt a = ConstInt Int

instance Functor ConstInt where
  fmap f (ConstInt x) = ConstInt x

instance Contravariant ConstInt where
  contramap f (ConstInt x) = ConstInt x


-- Elemi funktorok
--------------------------------------------------------------------------------

-- konstans funktor (olyan struktúra, ami nem használja a második
-- típusparamétert)
newtype Const a b = Const a

instance Functor (Const a) where
  fmap f (Const a) = Const a



-- identitásfunktor (egyelmű lista)
newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)



-- funktorok szorzása (pontonként), \f g x -> f x * g x
data Product f g a = Product (f a) (g a) deriving Show

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Product fa ga) = Product (fmap f fa) (fmap f ga)

-- példák:
prod1 :: Product [] [] Int
prod1 = Product [2, 3, 4] [4, 5, 6]

-- pl: fmap (\x -> x * x) prod1 == Product [4,9,16] [16,25,36]
prod2 :: Product Identity (Const Bool) Int
prod2 = Product (Identity 0) (Const False)
      -- Product (? :: Identity Int) (? :: Const Bool Int)


-- funktorok összeadása (pontonként)
data Sum f g a = SumLeft (f a) | SumRight (g a) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (SumLeft  fa) = SumLeft  (fmap f fa)
  fmap f (SumRight ga) = SumRight (fmap f ga)



-- két funktor komponálása
data Compose f g a = Compose (f (g a)) deriving Show

-- pl: Compose [] [] Int   ekvivalens azzal, hogy [[Int]]
comp1 :: Compose [] [] Int
comp1 = Compose [[3, 4], [4, 5, 6]]

-- hatványozás: szintén pontonként, de ez már nem funktor, mivel (f a)
-- fv. inputként szerepel data Exponent f g a = Exponent (f a -> g a)

--------------------------------------------------------------------------------

-- Funktor: mi a kategóriaelméleti definíció?

-- kategóriaelmélet: első definíció tankönyvben a kategória
--                   második a funktor

{-
Kategória komponensei:
- Egy halmaz, ami az objektumok halmaza
- Minden két objektum közötti nyilak (morfizmusok) halmazai
- Minden objektumból önmagába megy egy identitás-morfizmus
- Morfizmusokra van kompozíció művelet, ami asszociatív és
  az identitás-morfizmusok egységelemek rá.

Funktor: két kategória közötti művelettartó leképezés
- Minden objektumhoz hozzárendel egy objektumot
- Minden morfizmushoz rendel egy morfizmust a megfelelő objektuok között, ami
  + Identitásmorfizmushoz identitást rendel
  + Komponált morfizmushoz komponált morfizmust rendel

-}

-- típusok kategóriája: objektumok típusok, nyilak függvények

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
-- plusz igaz kell, hogy legyen:
--     fmap id      = id
--     fmap (f . g) = fmap f . fmap g
