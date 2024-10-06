import Data.Bits

apply :: (a -> b, a) -> b
apply = uncurry id

newtype LeftFunctor  a b x = LF ((x, a) -> b)

newtype RightFunctor a b x = RF (x -> (a -> b))

class Contravariant f where
  contramap :: (b -> a) -> (f a -> f b)

class Bifunctor f where
  bimap :: (a -> a') -> (b -> b') -> (f a b -> f a' b')

instance Bifunctor (,) where
  bimap g h (a, b) = (g a, h b)

instance Contravariant (LeftFunctor a b) where
  contramap g (LF f) = LF (f . bimap g id)

alpha :: forall a b x. LeftFunctor a b x -> RightFunctor a b x
alpha (LF f) = RF (curry f)

-- the inverse
alpha_1 :: forall a b x. RightFunctor a b x -> LeftFunctor a b x
alpha_1 (RF h) = LF (uncurry h)

q :: Int -> Bool
q n = n `mod` 2 == 0 

h :: (Int -> a) -> Bool -> a
h q' True  = q' 0
h q' False = q' 1

-- Exercises

q' :: Int -> Bool
q' x = testBit x 0
test1 = fmap (h q' . q) [-1, 0, 1, 2, 3]
test2 = fmap q' [-1, 0, 1, 2, 3]

-- The equalizer of id and reverse

q'' :: String -> Maybe Char
q'' s = if even len
       then Nothing
       else Just (s !! (len `div` 2))
  where len = length s
