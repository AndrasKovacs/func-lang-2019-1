

-- ajánlott online jegyzetek:
--      http://lambda.inf.elte.hu/
--           - Kezdő Haskell szekció:  magasabbrendű függvények
--           - Haladó Haskell szekció: Típusdefiníciók


-- 1. Definiáld újra a lista típust ADT-ként, "List" néven. Írj egy
--    "mapList :: (a -> b) -> List a -> List b", ami a lista minden elemére
--    egy függvényt alkalmaz.

data List a = Nil | Cons a (List a)

mapList :: (a -> b) -> List a -> List b
mapList f Nil         = Nil
mapList f (Cons a as) = Cons (f a) (mapList f as)


-- 2. Definiálj egy "BinTree" típust, aminek csak annotáció nélküli levelei és bináris elágazásai vannak.
--    Írj egy "numLeaves :: BinTree -> Int" függvényt, ami megszámolja a leveleket.


data BinTree = Leaf | Node BinTree BinTree

numLeaves :: BinTree -> Int
numLeaves Leaf       = 1
numLeaves (Node l r) = numLeaves l + numLeaves r


-- 3. Definiálj egy "Tree a" típust, aminek annotáció nélküli levelei és "a"-val annotált bináris
--    elágazásai vannak. Írj egy "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt, ami az elágazásokban
--    levő "a" értékekre egy függvényt alkalmaz.

data Tree a = TLeaf | TNode a (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f TLeaf         = TLeaf
mapTree f (TNode a l r) = TNode (f a) (mapTree f l) (mapTree f r)

-- alternatív definíció
mapTree' :: (a -> b) -> Tree a -> Tree b
mapTree' f = go where
  go TLeaf         = TLeaf
  go (TNode a l r) = TNode (f a) (go l) (go r)


-- 4. Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy listában található
--    minden függvényt alkalmaz egy értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".

applyMany :: [a -> b] -> a -> [b]
applyMany fs a = map (\f -> f a) fs

applyMany' :: [a -> b] -> a -> [b]
applyMany' fs a = map ($ a) fs


-- 5. Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár típusszinonímaként, aminek
--    az értékei nemüres listák. Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt,
--    ami nemüres listát ad vissza egy standard listából, ha az input nem üres.

-- egyszerű verzió
type NonEmptyList a = (a, [a])

fromList :: [a] -> Maybe (NonEmptyList a)
fromList []     = Nothing
fromList (a:as) = Just (a, as)

-- ADT verzió
data NonEmptyList' a = Single a | Cons' a (NonEmptyList' a)

fromList' :: [a] -> Maybe (NonEmptyList' a)
fromList' []     = Nothing
fromList' (a:as) = case fromList' as of
  Just as' -> Just (Cons' a as')
  Nothing  -> Just (Single a)
