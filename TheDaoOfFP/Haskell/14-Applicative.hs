class Monoidal f where
  unit  :: f ()
  (>*<) :: f a -> f b -> f (a, b)

