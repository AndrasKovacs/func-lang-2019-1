{-# language ViewPatterns, PatternSynonyms #-}

-- Pattern synonym + view pattern használata szebb [Bool] trie
-- implementációhoz.

-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-synonyms

-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#view-patterns
------------------------------------------------------------

data Tree a = Empty | Here a (Tree a) (Tree a) | Skip (Tree a) (Tree a)
   deriving (Eq, Show)

getNode :: Tree a -> Maybe (Maybe a, Tree a, Tree a)
getNode (Here a l r) = Just (Just a, l, r)
getNode (Skip l r)   = Just (Nothing, l, r)
getNode _            = Nothing

pattern Node :: Maybe a -> Tree a -> Tree a -> Tree a
pattern Node a l r <- (getNode -> Just (a, l, r)) where
  Node Nothing  Empty Empty = Empty
  Node Nothing  l     r     = Skip l r
  Node (Just a) l     r     = Here a l r

enlist :: Tree a -> [([Bool], a)]
enlist Empty        = []
enlist (Node a l r) = case a of
  Just a -> ([], a):rest
  _      -> rest
  where
    add b = map (\(bs, a) -> (b:bs, a))
    rest  = add True (enlist l) ++ add False (enlist r)

lookup' :: [Bool] -> Tree a -> Maybe a
lookup' _      Empty        = Nothing
lookup' []     (Node a l r) = a
lookup' (b:bs) (Node _ l r) = lookup' bs (if b then l else r)

insert :: [Bool] -> a -> Tree a -> Tree a
insert []     a Empty         = Here a Empty Empty
insert []     a (Node _ l r)  = Node (Just a) l r
insert (b:bs) a (Node a' l r) = if b then Node a' (insert bs a l) r
                                     else Node a' l (insert bs a r)
insert (b:bs) a Empty         = insert (b:bs) a (Skip Empty Empty)

delete :: [Bool] -> Tree a -> Tree a
delete _      Empty        = Empty
delete []     (Node _ l r) = Node Nothing l r
delete (b:bs) (Node a l r) = if b then Node a (delete bs l) r
                                  else Node a l (delete bs r)

tests = [
   enlist (Here 0 Empty Empty) == [([], 0)]
 , enlist (Skip (Here 0 Empty Empty) Empty) == [([True], 0)]
 , enlist (Skip (Skip (Here 0 Empty Empty) Empty) Empty) == [([True, True], 0)]
 , enlist (Skip Empty (Skip (Skip (Here 0 Empty Empty) Empty) Empty)) == [([False, True, True], 0)]
 , enlist (Empty :: Tree ()) == []
 , enlist (Skip Empty Empty :: Tree ()) == []
 , enlist (Here 0 (Here 1 Empty Empty) (Here 2 Empty Empty)) == [([],0),([True],1),([False],2)]
 , (delete [True, False, True] $ insert [True, False, True] 100 Empty) == Empty
 , (delete [True] $ insert [True, False, True] 100 Empty) == insert [True, False, True] 100 Empty
 , (lookup' [True, True, True] $ insert [True, True, True] 100 Empty) == Just 100
 , (lookup' [True, False, True] $ insert [True, True, True] 100 Empty) == Nothing
 , (delete (replicate 20 False) $ insert (replicate 20 False) 0 Empty) == Empty
 ]
