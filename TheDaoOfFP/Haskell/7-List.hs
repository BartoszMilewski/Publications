import Prelude hiding (foldr, map)

data List a where
  Nil  :: List a
  Cons :: (a, List a) -> List a

-- Elimination rule
recList :: c -> ((a, c) -> c) -> (List a -> c)
recList init step = \as ->
  case as of 
    Nil          -> init
    Cons (a, as) -> step (a, recList init step as)

foldr :: (a -> c -> c) -> c -> [a] -> c
foldr step init = \as ->
  case as of
    [] -> init
    a : as -> step a (foldr step init as)

sum :: [Nat] -> Nat
sum = foldr plus Z

-- From previous module

data Nat where
  Z :: Nat
  S :: Nat -> Nat

plus :: Nat -> Nat -> Nat
plus n m = case m of
  Z -> n
  (S k) -> S (plus k n)

-- Functoriality
mapList :: (a -> b) -> List a -> List b
mapList f = recList init step
  where
    init = Nil
    step (a, bs) = Cons (f a, bs)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (a : as) = f a : map f as

badMap :: (a -> b) -> [a] -> [b]
badMap f [] = []
badMap f (a : as) = badMap f as

test' = badMap id [1, 2, 3]

-- Exercises
h :: [a] -> Maybe a
h [] = Nothing
h (a : as) = Just a

h' :: [a] -> Maybe a
h' [] = Nothing
h' (a : as) = h as

third :: [a] -> Maybe a
third [] = Nothing
third (a : as) = second as
second :: [a] -> Maybe a
second [] = Nothing
second (a : as) = h as

test = third [1, 2, 3, 4]
