data Nat where
  Z :: Nat
  S :: Nat -> Nat

zero, one, two, three :: Nat
zero = Z
one  = S zero
two  = S one
three = S two

-- Elimination rule
rec :: a -> (a -> a) -> (Nat -> a)
rec init step = \n ->
  case n of
    Z     -> init
    (S m) -> step (rec init step m)

plus :: Nat -> Nat -> Nat
plus n = rec init step
  where
    init = n
    step = S

plus' :: Nat -> Nat -> Nat
plus' n m = case m of
  Z -> n
  (S k) -> S (plus' k n)

-- Exercises

plus'' :: Nat -> (Nat -> Nat)
plus'' n = rec init step n
  where
    init :: Nat -> Nat
    init = id
    -- Given a function that adds n to its argument
    -- Generate a function that adds (n + 1) to its argument
    step :: (Nat -> Nat) -> (Nat -> Nat)
    step f = S . f

toInt :: Nat -> Int
toInt n = rec init step n
  where
    init = 0
    step m = m + 1
test = toInt (plus'' two three)