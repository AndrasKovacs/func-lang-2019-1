
{-

-- Vizsgaidőpont
------------------------------------------------------------

- Kovács András: jún 10-14 nem jó

- 1. május 24 péntek     (16:00-kor kb.)
- 2. május 27 hétfő      (14:00-kor kb.)         (17:00-től nem jó)
- 3. TBA

-- Vizsga tartalma
------------------------------------------------------------

típusok
- nagyobb:
  - parser + eval + mtl, nagyobb feladat
- kisebb:
  - Functor, Foldable instance
  - lista/fa algoritmus (első gyak/hf feladatokhoz hasonló)
  - általános egyszerű State/Maybe/Reader monád feladat

-}





-- memoization
--------------------------------------------------------------------------------

-- 1. primkó memoization
-- 2. generikus trie memoization

-- alapok

-- exponenciális
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

-- memoizált
-- fibs :: [Int]
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
           -- THUNK:
           --    pointer (zipWith (+))-ra  | üres 64 bit

-- hasonló problémák: coin change, Levenhstein distance
--    (dinamikus programozás / rekurrencia relációs algoritmus)
--    (backpack probléma dinakimus prog. megoldása)

-- dinamikus programozás mutáció nélkül?
-- lusta kiértékelés: lehet mutálni, de minden referenciát max 1 szer.
-- (lineáris típusozás)  (Rust: affin típusozás (lineáris))

-- data Tree = Leaf Int | Node Tree Tree deriving Show

-- maxes :: Tree -> Tree
-- maxes t = t' where
--   (globalM, t') = go t (0 :: Int)
--   go (Leaf n)   m = (max n m, Leaf globalM)
--   go (Node l r) m = (m'', Node l' r') where
--     (m' , l') = go l m
--     (m'', r') = go r m'

-- -- AVL tree
-- data Tree k a = Leaf a | Node !Int k (Tree a) (Tree a)


------------------------------------------------------------

fibs = map fib' [0..]

fib' :: Int -> Int
fib' 0 = 1
fib' 1 = 1
fib' n = fibs !! (n - 2) + fibs !! (n - 1)

------------------------------------------------------------

-- fix :: (a -> a) -> a
--     :: ((Int -> Int) -> Int -> Int) -> Int -> Int


fix :: (a -> a) -> a
fix f = let x = f x in x  -- standard lib definíció (hatékony)
   -- = f (fix f)
   -- függvény fixpontja:  f x = x

fibNonMemo = fix fibf


fibf :: (Int -> Int) -> Int -> Int
fibf recurse = go where
  go 0 = 1
  go 1 = 1
  go n = recurse (n - 2) + recurse (n - 1)


memofix :: ((Int -> Int) -> Int -> Int) -> Int -> Int
memofix f = go where
  memoList = map go [0..]
  go = f (memoList !!)

------------------------------------------------------------

-- 1. feladat: ha a kulcs Bool, akkor mi a (Map Bool v?)

type MapBool v = (v, v)

-- (k -> v)
-- (Bool -> v)
-- v^2 ~ v * v ~ (v, v)

-- ((a, b) -> v)
-- v^(a*b) ~ (v^a)^b  ~ (a -> (b -> v))

-- (Either a b -> v)
-- v^(a+b) ~ v^a * v^b  ~ (a -> v, b -> v)

-- (() -> v) ~ v

------------------------------------------------------------

-- kulcs: Nat, vagy Tree

-- feladat: mi az az adatstruktúra, aminek a kulcsa bináris leveles fa

data Tree = Leaf | Node Tree Tree deriving Show

-- type MapTree v = ?

data MapTree v = MapTree v (MapTree (MapTree v))  -- "nested type"

lookupTree :: Tree -> MapTree v -> v
lookupTree Leaf       (MapTree v _) = v
lookupTree (Node l r) (MapTree _ m) = lookupTree r (lookupTree l m)

m1 :: MapTree Int
m1 = MapTree 10 (MapTree undefined (MapTree undefined undefined))


-- String trie

-- data Str = Nil | Cons Str Char
-- String trie struktúra mit tud, amit a HashTable nem?

-- Scrabble AI

-- (a : A) -> B a  -- indexált szorzat
-- Σ A λ a -> B a  -- indexált összeg
