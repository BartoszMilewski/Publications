import Prelude hiding (Functor, fmap)
import Data.Kind ( Type )

class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

instance Functor Maybe where
  fmap g Nothing  = Nothing
  fmap g (Just a) = Just (g a)

data WithInt a = WithInt a Int

instance Functor WithInt where
    fmap f (WithInt a n) = WithInt (f a) n

newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

newtype Constant c a = Constant c

instance Functor (Constant c) where
    fmap f (Constant c) = (Constant c)

class Bifunctor f where
  bimap :: (a -> a') -> (b -> b') -> (f a b -> f a' b')

instance Bifunctor (,) where
  bimap g h (a, b) = (g a, h b)

data MoreThanA a b = More a (Maybe b)

instance Bifunctor MoreThanA where
    bimap g h (More a Nothing) = More (g a) Nothing
    bimap g h (More a (Just b)) = More (g a) (Just (h b))

class Contravariant f where
  contramap :: (b -> a) -> (f a -> f b)

newtype Predicate a = Predicate (a -> Bool)

instance Contravariant Predicate where
  contramap f (Predicate h) = Predicate (h . f)

newtype Tester a = Tester ((a -> Bool) -> Bool)

instance Functor Tester where
  fmap f (Tester g) = Tester g'
    where g' h = g (h . f)

class Profunctor f where
  dimap :: (a' -> a) -> (b -> b') -> (f a b -> f a' b')

instance Profunctor (->) where
  dimap f g h = g . h . f

newtype Compose' g f a = Compose' (g (f a))

data Compose :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) 
  where
    Compose :: (g (f a)) -> Compose g f a

instance (Functor g, Functor f) => Functor (Compose g f) where
  fmap h (Compose gfa) = Compose (fmap (fmap h) gfa)

instance (Functor g, Contravariant f) => Contravariant (Compose g f) where
    contramap h (Compose gfa) = Compose (fmap (contramap h) gfa)
