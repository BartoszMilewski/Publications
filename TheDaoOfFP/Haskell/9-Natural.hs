import Data.Kind (Type)
import Prelude hiding (reverse)
import GHC.CmmToAsm.AArch64.Instr (x0)

data Natural' :: (Type -> Type) -> (Type -> Type) -> Type where
  Natural' :: (forall a. f a -> g a) -> Natural' f g

type Natural f g = forall a. f a -> g a

oneWay :: 
  forall f g a b. (Functor f, Functor g) => 
  Natural f g -> (a -> b) -> f a -> g b
oneWay alpha h = fmap @g h . alpha @a

otherWay :: 
  forall f g a b. (Functor f, Functor g) => 
  Natural f g -> (a -> b) -> f a -> g b
otherWay alpha h = alpha @b . fmap @f h

safeHead :: Natural [] Maybe
safeHead [] = Nothing
safeHead (a : as) = Just a

reverse :: Natural [] []
reverse [] = []
reverse (a : as) = reverse as ++ [a]

-- Horizontal composition

data F  x
data F' x 
data G  x
data G' x 
instance Functor G' where
    fmap = undefined 
instance Functor G where
    fmap = undefined 


alpha :: forall x. F x -> F' x
alpha = undefined

beta  :: forall x. G x -> G' x
beta = undefined

beta_alpha :: forall x. G (F x) -> G' (F' x)
beta_alpha = beta . fmap alpha

beta_alpha' = fmap alpha . beta

-- Whiskering

beta_f :: forall x. G (F x) -> G' (F x)
beta_f = beta

h_alpha :: forall x. H (G x) -> H (G' x)
h_alpha = fmap alpha
