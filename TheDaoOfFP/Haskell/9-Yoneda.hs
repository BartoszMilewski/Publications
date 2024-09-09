yoneda :: Functor f => (forall x. (a -> x) -> f x) -> f a
yoneda g = g id

-- the inverse to yoneda
yoneda_1 :: Functor f => f a -> (forall x. (a -> x) -> f x)
yoneda_1 y = \h -> fmap h y

class Contravariant f where
    contramap :: (b -> a) -> f a -> f b

coyoneda :: Contravariant f => (forall x. (x -> a) -> f x) -> f a
coyoneda g = g id

-- the inverse to coyoneda
coyoneda_1 :: Contravariant f => f a -> (forall x. (x -> a) -> f x)
coyoneda_1 y = \h -> contramap h y


toNatural :: (x -> y) -> (forall z. (z -> x) -> (z -> y))
toNatural f = \h -> f . h 

toNatural' :: (x -> y) -> (forall z. (z -> x) -> (z -> y))
toNatural' f = (f . )

fromNatural :: (forall z. (z -> x) -> (z -> y)) -> (x -> y)
fromNatural alpha = alpha id
