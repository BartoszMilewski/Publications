data Void
   -- Void has no constructor

-- Unique arrow from the initial Void to any type a
-- Since Void has no constructor, 
-- no element of Void can be passed to this function
absurd :: Void -> a
absurd v = undefined

-- Unique arrow from any type a to () -- the terminal type
unit :: a -> ()
unit a = ()

-- x is an element of the type Int
-- it's value is 42
x :: Int
x = 42
-- Categorically it's equivalent to:
y :: () -> Int
y () = 42
