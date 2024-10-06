import Prelude hiding (either)

-- Universal property of sum
mapOut :: (a -> c, b -> c) -> (Either a b -> c)
mapOut (f, g) = \aorb -> case aorb of
                         Left  a -> f a
                         Right b -> g b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y

unEither :: (Either a b -> c) -> (a -> c, b -> c)
unEither h = (h . Left, h . Right)

-- Universal property of product
mapIn :: (c -> a, c -> b) -> (c -> (a, b))
mapIn (f, g) = \c -> (f c, g c)

(&&&) :: (c -> a) -> (c -> b) -> (c -> (a, b))
(f &&& g) c = (f c, g c)

fork :: (c -> (a, b)) -> (c -> a, c -> b)
fork h = (fst . h, snd . h)

-- Functoriality of sum
h :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
h f g = either (Left . f) (Right . g)

bimap :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
bimap f g (Left  a) = Left  (f a)
bimap f g (Right b) = Right (g b)


-- Functoriality of product
h' :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
h' f g = (f . fst) &&& (g . snd)

bimap' :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
bimap' f g (a, b) = (f a, g b)

-- Functoriality of function type
dimap :: (a' -> a) -> (b -> b') -> (a -> b) -> (a' -> b')
dimap f g h = g . h . f

-- Distributivity
dist'' :: Either (b, a) (c, a) -> (Either b c, a)
dist'' = either f g
  where
    f   :: (b, a) -> (Either b c, a)
    f = f' &&& f''
    g   :: (c, a) -> (Either b c, a)
    g   = g' &&& g''
    f'  :: (b, a) -> Either b c
    f'  = Left . fst
    g'  :: (c, a) -> Either b c
    g'  = Right . fst
    f'' :: (b, a) -> a
    f'' = snd
    g'' :: (c, a) -> a
    g'' = snd

dist' :: Either (b, a) (c, a) -> (Either b c, a)
dist' = either ((Left . fst) &&& snd) ((Right . fst) &&& snd)

dist :: Either (b, a) (c, a) -> (Either b c, a)
dist (Left  (b, a)) = (Left  b, a)
dist (Right (c, a)) = (Right c, a)

undist :: (Either b c, a) -> Either (b, a) (c, a)
undist (Left b, a)  = Left (b, a)
undist (Right c, a) = Right (c, a)

undist' = uncurry (either (curry Left) (curry Right))

-- Exercises
-- 2 x a = a + a
toSum :: (Bool, a) -> Either a a
toSum (True, a)  = Left a
toSum (False, a) = Right a

fromSum :: Either a a -> (Bool, a)
fromSum (Left a)  = (True, a)
fromSum (Right a) = (False, a)