class Monoidal f where
  unit  :: f ()
  (>*<) :: f a -> f b -> f (a, b)

data FreeA f x where
  DoneA :: x -> FreeA f x
  MoreA :: ((a, b) -> x) -> f a -> FreeA f b -> FreeA f x

instance Functor f => Functor (FreeA f) where
  fmap :: Functor f => (x -> x') -> FreeA f x -> FreeA f x'
  fmap h (DoneA x) = DoneA (h x)
  fmap h (MoreA ab_x fa free_b) = MoreA (h . ab_x) fa free_b

instance Functor f => Monoidal (FreeA f) where
  unit = DoneA ()
  (DoneA x) >*< fry = fmap (x,) fry
  (MoreA abx fa frb) >*< fry = MoreA (reassoc abx) fa (frb >*< fry)

reassoc :: ((a, b)-> x) -> (a, (b, y)) -> (x, y)
reassoc abx (a, (b, y)) = (abx (a, b), y)

instance Functor f => Applicative (FreeA f) where
  pure = DoneA
  ff <*> fx = fmap app (ff >*< fx)
  
app :: (a -> b, a) -> b
app (f, a) = f a
