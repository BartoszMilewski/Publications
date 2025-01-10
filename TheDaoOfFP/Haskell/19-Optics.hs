class Profunctor p where
    dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'

type Iso s t a b = (s -> a, b -> t)

type IsoP s t a b = forall p. Profunctor p => p a b -> p s t

toIsoP :: (s -> a, b -> t) -> IsoP s t a b
toIsoP (f, g) = dimap f g

class Profunctor p => Cartesian p where
  alpha :: p a b -> p (c, a) (c, b)

data LensE s t a b where
  LensE :: (s -> (c, a)) -> ((c, b) -> t) -> LensE s t a b

type LensP s t a b = forall p. Cartesian p => p a b -> p s t

toLensP :: LensE s t a b -> LensP s t a b
toLensP (LensE from to) = dimap from to . alpha

data FlipLens a b s t = FlipLens (s -> a) (s -> b -> t)

instance Profunctor (FlipLens a b) where
  dimap f g (FlipLens get set) = FlipLens (get . f) (fmap g . set . f)

instance Cartesian (FlipLens a b) where
  alpha(FlipLens get set) = FlipLens get' set'
    where get' = get . snd
          set' = \(x, s) b -> (x, set s b)

fromLensP :: LensP s t a b -> (s -> a, s -> b -> t)
fromLensP pp = (get', set')
  where FlipLens get' set' = pp (FlipLens id (\s b -> b))

-- Exercise solutions

fromIsoP :: IsoP s t a b -> (s -> a, b -> t)
fromIsoP h = unAd (h (Ad (id, id)))

newtype Adapter a b s t = Ad (s -> a, b -> t)

unAd :: Adapter a b s t -> (s -> a, b -> t)
unAd (Ad p) = p

instance Profunctor (Adapter a b) where
    dimap f g (Ad (h, h')) = Ad (h . f, g . h')

data Prism s t a b where
  Prism :: (s -> Either c a) -> (Either c b -> t) -> Prism s t a b

toMatch :: Prism s t a b -> (s -> Either t a)
toMatch (Prism from to) s =
  case from s of
    Left  c -> Left (to (Left c))
    Right a -> Right a

toBuild :: Prism s t a b -> (b -> t)
toBuild (Prism from to) b = to (Right b)

toPrism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
toPrism match build = Prism from to
  where
    from = match
    to (Left  c) = c
    to (Right b) = build b

class Profunctor p => Cocartesian p where
  alpha' :: p a b -> p (Either c a) (Either c b)

type PrismP s t a b = forall p. Cocartesian p => p a b -> p s t

toPrismP :: Prism s t a b -> PrismP s t a b
toPrismP (Prism from to) = dimap from to . alpha'

type Traversal s t a b = s -> ([b] -> t, [a])
