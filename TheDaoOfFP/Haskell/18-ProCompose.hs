class Profunctor p where
  dimap :: (s -> a) -> (b -> t) -> (p a b -> p s t)

data Procompose p q a b where
  Procompose ::  q a x -> p x b -> Procompose p q a b

mapOut :: Procompose p q a b -> (forall x. q a x -> p x b -> c) -> c
mapOut (Procompose qax pxb) f = (f qax pxb)

instance (Profunctor p, Profunctor q) => Profunctor (Procompose p q) 
  where
    dimap l r (Procompose qax pxb) = 
               Procompose (dimap l id qax) (dimap id r pxb)

