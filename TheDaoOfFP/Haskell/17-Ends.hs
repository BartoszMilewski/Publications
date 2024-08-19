
type End p = forall x. p x x

data Coend p where
  Coend ::  p x x -> Coend p

type Natural f g = forall x. f x -> g x

data Yo f a x y = Yo ((a -> x) -> f y)

yoneda :: Functor f => End (Yo f a) -> f a
yoneda (Yo g) = g id

yoneda_1 :: Functor f => f a -> End (Yo f a)
yoneda_1 fa = Yo (\h -> fmap h fa)

data CoY f a x y = CoY (x -> a) (f y)

coyoneda :: Functor f => Coend (CoY f a) -> f a
coyoneda (Coend (CoY g fa)) = fmap g fa

coyoneda_1 :: Functor f => f a -> Coend (CoY f a)
coyoneda_1 fa = Coend (CoY id fa)

data Day f g x where
  Day :: ((a, b) -> x) -> f a -> g b -> Day f g x

instance Functor (Day f g) where
    fmap :: (a -> b) -> Day f g a -> Day f g b
    fmap h (Day xy_a fx gy) = Day (h . xy_a) fx gy

assoc :: Day f (Day g h) x -> Day (Day f g) h x
-- result  Day yd_x (Day ac_y fa gc) hd
-- pick y = (a, c)
assoc (Day ab_x fa (Day cd_b gc hd)) = 
       Day (\((a, c), d) -> ab_x (a, cd_b (c, d))) (Day id fa gc) hd
