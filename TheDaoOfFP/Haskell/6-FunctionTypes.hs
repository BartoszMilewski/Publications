import Data.Complex

curry   :: ((c, a) -> b)   -> (c -> (a -> b))
curry f = \c -> (\a -> f (c, a))

uncurry :: (c -> (a -> b)) -> ((c, a) -> b)
uncurry f = \(c, a) -> f c a

apply :: (a -> b, a) -> b
apply (f, x) = f x

($) :: (a -> b) -> a -> b
f $ x = f x

pair :: a -> b -> (a, b)
pair a b = (a, b)

pairWithTen :: a -> (Int, a)
pairWithTen = pair 10 -- partial application of pair

type C = Complex Double

h :: (Double, Double, Double) -> (C -> C)
h (a, b, c) = \x -> (a :+ 0) * x * x + (b :+ 0) * x + (c :+ 0)
