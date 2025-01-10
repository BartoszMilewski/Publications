-- Right Kan extension of f along p

newtype Ran p f b = Ran (forall e. (b -> p e) -> f e)

counit :: forall p f e'. Ran p f (p e') -> f e'
counit (Ran h) = h id

-- Universal property

type Alpha p f g = forall e. g (p e) -> f e

sigma :: Functor g => Alpha p f g -> forall b. (g b -> Ran p f b)
sigma alpha gb = Ran (\b_pe -> alpha $ fmap b_pe gb)

factorize :: Functor g => Alpha p f g -> forall e. g (p e) -> f e
factorize alpha = counit . sigma alpha

-- By eqiational reasoning, this is equivalent to
factorize' :: Functor g => Alpha p f g -> forall e. g (p e) -> f e
factorize' alpha = alpha

-- Codensity monad

newtype Codensity f c = C (forall d. (c -> f d) -> f d)

runCodensity :: Codensity f c -> forall d. (c -> f d) -> f d
runCodensity (C h) = h

-- Exercises

instance Functor (Ran p f) where
   fmap :: (b -> b') -> Ran p f b -> Ran p f b'
   fmap g (Ran k) = Ran (\b'_pe -> k (b'_pe . g))

instance Functor (Codensity f) where
   fmap h (C k) = C (\c_fd -> k (c_fd . h))

instance Applicative (Codensity f) where
   pure :: c -> Codensity f c
   pure x = C (\k -> k x)
   (<*>) :: Codensity f (a -> b) -> Codensity f a -> Codensity f b
   C k <*> C k' = C (\b_fd -> k (\ab -> k' (b_fd . ab)))

instance Monad (Codensity f) where
  return = pure
  m >>= kl = C (\k -> runCodensity m (\a -> runCodensity (kl a) k))
