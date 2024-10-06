-- We want to use our own definitions here
import Prelude hiding (Either, Left, Right, Maybe, Nothing, Just)

-- We primed some definition to avoid name conflicts
data Bool' where
  True'  :: () -> Bool'
  False' :: () -> Bool'

data Bool'' where
  True''  :: Bool''
  False'' :: Bool''

x' :: Bool
x' = True

type A = Int -- any type will do

x :: A
x = 5
y :: A
y = 7
z :: A
z = 11

h :: Bool -> A
h b = if b then x else y

not :: Bool -> Bool
not b = if b then False else True

data RGB where
  Red   :: RGB
  Green :: RGB
  Blue  :: RGB

c :: RGB
c = Blue

h1 :: RGB -> A
h1 Red   = x
h1 Green = y
h1 Blue  = z

h2 :: Bool -> A
h2 True  = x
h2 False = y

h3 :: RGB -> A
h3 c = case c of
  Red   -> x
  Green -> y
  Blue  -> z

h4 :: Bool -> A
h4 b = case b of
  True  -> x
  False -> y

c' :: Char
c' = 'a'

yesno :: Char -> Bool
yesno c = case c of
  'y' -> True
  'Y' -> True
  _   -> False

data Either a b where
  Left  :: a -> Either a b
  Right :: b -> Either a b

-- Placate the compiler with 'undefined'
f :: a -> c
f = undefined
g :: b -> c
g = undefined

h5 :: Either a b -> c
h5 (Left  a) = f a
h5 (Right b) = g b

h6 :: Either a b -> c
h6 e = case e of
  Left  a -> f a
  Right b -> g b

data Maybe' a where
  Nothing' :: () -> Maybe' a
  Just'    ::  a -> Maybe' a

data Maybe a = Nothing | Just a

data Void
absurd :: a -> Void
absurd a = undefined

f' :: Either () Void -> ()
f' (Left ()) = ()
f' (Right _) = ()

f_1 :: () -> Either () Void
f_1 _ = Left ()

-- Exercises

from :: Either a Void -> a
from (Left a) = a
from (Right v) = undefined -- will never be called

to :: a -> Either a Void
to x = Left x

sym :: Either a b -> Either b a
sym (Left a) = Right a
sym (Right b) = Left b

f1 :: a -> x
f1 = undefined
f2 :: b -> x
f2 = undefined
f3 :: c -> x
f3 = undefined

h7 :: Either (Either a b) c -> x
h7 (Left (Left a))  = f1 a
h7 (Left (Right b)) = f2 b
h7 (Right c)        = f3 c

h7' :: Either a (Either b c) -> x
h7' (Left a)          = f1 a
h7' (Right (Left b))  = f2 b
h7' (Right (Right c)) = f3 c
