class Profunctor p where
  dimap :: (s -> a) -> (b -> t) -> (p a b -> p s t)

data Coend p where
  Coend ::  p x x -> Coend p

newtype ProPair q p a b x y = ProPair (q a y, p x b)

instance (Profunctor p, Profunctor q) => Profunctor (ProPair q p a b) where
    dimap :: (Profunctor p, Profunctor q) => (x' -> x)-> (y -> y') -> 
              ProPair q p a b x y -> ProPair q p a b x' y'
    dimap l r (ProPair (qay, pxb)) = ProPair (dimap id r qay, dimap l id pxb)

newtype CoEndCompose p q a b = CoEndCompose (Coend (ProPair q p a b))

instance (Profunctor p, Profunctor q) => Profunctor (CoEndCompose p q) where
    dimap :: (Profunctor p, Profunctor q) => (s -> a) -> (b -> t) -> 
              CoEndCompose p q a b -> CoEndCompose p q s t
    dimap l r (CoEndCompose (Coend (ProPair (qay, pxb)))) = 
        CoEndCompose (Coend (ProPair (dimap l id qay, dimap id r pxb)))

