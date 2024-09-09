--import Data.Functor.Identity
import Prelude hiding(reverse)
import Data.Bool (bool)

-- Cayley

reverse :: [a] -> [a]
reverse [] = []
reverse (a : as) = reverse as ++ [a]

{- Imported from Prelude
instance Monoid [a] where
  mempty = []
  mappend as bs = as ++ bs
-}

type DList a = [a] -> [a]

rep :: [a] -> DList a
rep as = \xs -> as ++ xs

unRep :: DList a -> [a]
unRep f = f []

-- rep [] = id
-- rep (xs ++ ys) = rep xs . rep ys

rev :: [a] -> DList a
rev [] = rep []
rev (a : as) = rev as . rep [a]

fastReverse :: [a] -> [a]
fastReverse = unRep . rev

test2 = take 10 $ reverse [1..10000000]
test3 = take 10 $ fastReverse [1..10000000]

reverse' :: [a] -> [a]
reverse' = foldl (\as a -> a : as) []

-- Tannaka

data Identity a = Identity a 
  
runIdentity :: Identity a -> a
runIdentity (Identity a) = a

instance Functor Identity where
  fmap g (Identity a) = Identity (g a)

toTannaka :: (a -> b) -> (forall f. Functor f => f a -> f b)
toTannaka g fa = fmap g fa

fromTannaka :: (forall f. Functor f => f a -> f b) -> (a -> b)
fromTannaka g a = runIdentity (g (Identity a))

type Getter a b = forall f. Functor f => f a -> f b

boolToStrGetter :: Getter Bool String
boolToStrGetter = toTannaka show . toTannaka (bool (-1) 1)

test1 :: String
test1 = (fromTannaka boolToStrGetter) False
