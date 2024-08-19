class Profunctor p where
  dimap :: (s -> a) -> (b -> t) -> (p a b -> p s t)

class Profunctor p => PreArrow p where
  (>>>) :: p a x -> p x b -> p a b
  arr   :: (a -> b) -> p a b

class PreArrow p => Arrow p where
    first :: p a b -> p (a, c) (b, c)
