thrd :: (a, b, c) -> c
thrd (_, _, c) = c

data Product a b = Pair { fst' :: a, snd' :: b }

ic :: Product Int Char
ic = Pair 10 'A'

swap' :: (a, b) -> (b, a)
swap' x = (snd x, fst x)

swap (x, y) = (y, x)

assoc :: ((a, b), c) -> (a, (b, c))
assoc ((a, b), c) = (a, (b, c))

runit :: (a, ()) -> a
runit (a, _) = a

class Monoid m where
  mappend :: (m, m) -> m
  mempty  :: () -> m


-- Exercises

maybeAB :: Either b (a, b) -> (Maybe a, b)
maybeAB (Left b) = (Nothing, b)
maybeAB (Right (a, b)) = (Just a, b)
-- Another possibility:
-- maybeAB (Right (a, b)) = (Nothing, b)