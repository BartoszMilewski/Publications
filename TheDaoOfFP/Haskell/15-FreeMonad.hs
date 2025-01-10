import Data.Kind ( Type )
import Data.Functor.Const

type Natural f g = forall a. f a -> g a

class HFunctor (hf :: (Type -> Type) -> Type -> Type) where
   hmap :: (Functor f, Functor g) => 
       Natural f g -> Natural (hf f) (hf g)

data Phi f g a where
   IdF :: a -> Phi f g a
   CompF :: f (g a) -> Phi f g a

instance Functor f => HFunctor (Phi f) where
   hmap :: (Functor f, Functor g, Functor h) =>
          Natural g h -> Natural (Phi f g) (Phi f h)
   hmap alpha (IdF a) = IdF a
   hmap alpha (CompF fga) = CompF (fmap alpha fga)

instance (Functor f, Functor g) => Functor (Phi f g) where
   fmap h (IdF a) = IdF (h a)
   fmap h (CompF fga) = CompF (fmap (fmap h) fga)

data FreeMonad f a where
   Pure :: a -> FreeMonad f a
   Free :: f (FreeMonad f a) -> FreeMonad f a

instance HFunctor FreeMonad where
   hmap :: (Functor f, Functor g) =>
      Natural f g -> Natural (FreeMonad f) (FreeMonad g)
   hmap _ (Pure a) = Pure a
   hmap alpha (Free ffa) = Free (alpha (fmap (hmap alpha) ffa))

instance (Functor f) => Functor (FreeMonad f) where
   fmap h (Pure a) = Pure (h a)
   fmap h (Free ffa) = Free (fmap (fmap h) ffa)

eta :: a -> FreeMonad f a
eta = Pure

mu :: Functor f => FreeMonad f (FreeMonad f a) -> FreeMonad f a
mu (Pure fa) = fa
mu (Free ffa) = Free (fmap mu ffa)

instance Functor f => Applicative (FreeMonad f) where
   pure = eta
   Pure f  <*> Pure a = Pure (f a)
   Pure f  <*> Free ffa = Free (fmap (fmap f) ffa)
   Free ff <*> fa = Free (fmap (<*> fa) ff)

instance Functor f => Monad (FreeMonad f) where
   return = pure
   (Pure a)   >>= k = k a
   (Free ffa) >>= k = Free (fmap (>>= k) ffa)
   -- or, equivalently, using mu:
   -- m >>= k = mu (fmap k m)

type MAlg f g a = (a -> g a, f (g a) -> g a)

mcata :: Functor f => MAlg f g a -> FreeMonad f a -> g a
mcata (l, r) (Pure a) = l a
mcata (l, r) (Free ffa) = 
  r (fmap (mcata (l, r)) ffa)

-- Stack calculator

data StackF k  = Push Int k
               | Top (Int -> k)
               | Pop k            
               | Add k
               deriving Functor

type FreeStack = FreeMonad StackF

liftF :: (Functor f) => f r -> FreeMonad f r
liftF fr = Free (fmap Pure fr)

push :: Int -> FreeStack ()
push n = liftF (Push n ())

pop :: FreeStack ()
pop = liftF (Pop ())

top :: FreeStack Int
top = liftF (Top id)

add :: FreeStack ()
add = liftF (Add ())

calc :: FreeStack Int
calc = do
  push 3
  push 4
  add
  x <- top
  pop
  return x

newtype StackAction k = St ([Int] -> ([Int], k))
  deriving Functor

runAction :: StackAction k -> [Int] -> ([Int], k)
runAction (St act) ns = act ns

runAlg :: MAlg StackF StackAction a
runAlg = (stop, go)
  where
   stop :: a -> StackAction a
   stop a = St (\xs -> (xs, a))

   go :: StackF (StackAction k) -> StackAction k
   go (Pop k)    = St (\ns -> runAction k (tail ns))
   go (Top ik)   = St (\ns -> runAction (ik (head ns)) ns)
   go (Push n k) = St (\ns -> runAction k (n: ns))
   go (Add k)    = St (\ns -> runAction k 
                     ((head ns + head (tail ns)): tail (tail ns)))

run :: FreeMonad StackF k -> ([Int], k)
run prog = runAction (mcata runAlg prog) [] 

test1 :: ([Int], Int)
test1 = run calc

-- Exercises

data Rose a = Leaf a | Rose [Rose a]
  deriving Functor

roseToFree :: Rose a -> FreeMonad [] a
roseToFree (Leaf a)  = Pure a
roseToFree (Rose rs) = Free (fmap roseToFree rs)

freeToRose :: FreeMonad [] a -> Rose a
freeToRose (Pure a)  = Leaf a
freeToRose (Free as) = Rose (fmap freeToRose as)

data Bin a = Bin a a

data Tree a = Tip a | Branch (Tree a) (Tree a)

treeToFree :: Tree a -> FreeMonad Bin a
treeToFree (Tip a) = Pure a
treeToFree (Branch left right) = Free (Bin (treeToFree left) (treeToFree right))

freeToTree :: FreeMonad Bin a -> Tree a
freeToTree (Pure a) = Tip a
freeToTree (Free (Bin left right)) = Branch (freeToTree left) (freeToTree right)

showAlg :: MAlg StackF (Const String) a
showAlg = (stop, go)
   where stop :: a -> Const String a
         stop a = Const "Stop! "
         go :: StackF (Const String a) -> Const String a
         go (Push n k) = Const ("Push " ++ show n ++ " " ++ getConst k)
         go (Top k)    = Const ("Top " ++ getConst (k 42)) -- dummy
         go (Pop k)    = Const ("Pop " ++ getConst k)
         go (Add k)    = Const ("Add " ++ getConst k)

test2 = getConst $ mcata showAlg calc
