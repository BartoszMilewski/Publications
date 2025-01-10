-- Left Kan extension of f along p
data Lan p f b where
   Lan :: (p e -> b) -> f e -> Lan p f b

unit :: forall p f e'. f e' -> Lan p f (p e')
unit fe = Lan id fe 

-- Universal property

type Alpha p f g = forall e. f e -> g (p e)

sigma :: Functor g => Alpha p f g -> forall b. (Lan p f b -> g b)
sigma alpha (Lan pe_b fe) = fmap pe_b (alpha fe)

factorize :: Functor g => Alpha p f g -> f e -> g (p e)
factorize alpha = sigma alpha . unit

-- Exercise

instance Functor (Lan p f) where
    fmap :: (b -> b') -> Lan p f b -> Lan p f b'
    fmap g (Lan pe_b fe) = Lan (g . pe_b) fe

data Density f c where
   D :: (f d -> c) -> f d -> Density f c

class Comonad w where
   extract :: w c -> c
   duplicate :: w c -> w (w c)

instance Comonad (Density f) where
   extract :: Density f c -> c
   extract (D fd_c fd) = fd_c fd
   duplicate :: Density f c -> Density f (Density f c)
   duplicate (D fd_c fd) = D (D fd_c) fd
