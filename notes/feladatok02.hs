-- notes/feladatok02.hs

-- GHCI használata:
-- :t    kifejezés típusát lekérdezni
-- :r    újratölteni fájlt
-- :i    információ (pl típusosztályoknál: instane-ok method-ok listázása)


-- 1. Definiáld a binárisan ágazó leveles fák típusát "Tree a"-ként. Minden
--    levélen legyen egy "a" annotáció.
--      - Definiáld a "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt.
--      - Definiáld az Eq (Tree a) instance-ot.

-- 2. Definiáld a "partition :: (a -> Bool) -> [a] -> ([a], [a])" függvényt, ami
--    az első output listában visszaadja azokat az elemeket, amelyekre az "f :: a -> Bool"
--    igaz, a második output-ban pedig azokat, amire "f" hamis.

-- 3. Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
--    az összes bemenő függvény kompozíciója,
--    pl. "composeAll [f, g, h] x == f (g (h x))"

-- 4. Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
--    rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.

-- 5. Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
--    iterált felhasználásával rendez egy listát.

-- 6. Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
--    minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
--    [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
--    fontos, hogy az össze részlista szerepeljen.
