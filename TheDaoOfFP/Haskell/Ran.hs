-- Right Kan extension of f along p

newtype Ran p f b = Ran (forall c. (b -> p c) -> f c)

counit :: forall p f c'. Ran p f (p c') -> f c'
counit (Ran h) = h id

-- Universal property

type Alpha p f g = forall c. g (p c) -> f c

sigma :: Functor g => Alpha p f g -> forall b. (g b -> Ran p f b)
sigma alpha gb = Ran (\b_pc -> alpha $ fmap b_pc gb)

factorize :: Functor g => Alpha p f g -> forall c. g (p c) -> f c
factorize alpha = counit . sigma alpha

-- Exercise

instance Functor (Ran p f) where
   fmap :: (b -> b') -> Ran p f b -> Ran p f b'
   fmap g (Ran k) = Ran (\b'_pc -> k (b'_pc . g))