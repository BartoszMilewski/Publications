-- Exercise
type DayHom g h a = forall b. g b -> h (a, b)

data Day f g a where
    Day :: ((x, y) -> a) -> f x -> g y -> Day f g a 

-- Witnesses of the adjunction in the functor category

ltor :: (forall a. Day f g a -> h a) -> (forall a. f a -> DayHom g h a)
ltor day_h fa = \gb -> day_h (Day (uncurry (,)) fa gb)

rtol :: Functor h => (forall a. f a -> DayHom g h a) -> (forall a. Day f g a -> h a)
rtol f_hom (Day xy_a fx gy) = fmap xy_a (f_hom fx gy)