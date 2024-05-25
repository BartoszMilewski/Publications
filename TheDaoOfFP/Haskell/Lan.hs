-- Left Kan extension of f along p
data Lan p f b where
   Lan :: (p c -> b) -> f c -> Lan p f b

unit :: forall p f c'. f c' -> Lan p f (p c')
unit fc = Lan id fc 

-- Universal property

type Alpha p f g = forall c. f c -> g (p c)

sigma :: Functor g => Alpha p f g -> forall b. (Lan p f b -> g b)
sigma alpha (Lan pc_b fc) = fmap pc_b (alpha fc)

factorize :: Functor g => Alpha p f g -> f c -> g (p c)
factorize alpha = sigma alpha . unit

-- Exercise

instance Functor (Lan p f) where
    fmap :: (b -> b') -> Lan p f b -> Lan p f b'
    fmap g (Lan pc_b fc) = Lan (g . pc_b) fc