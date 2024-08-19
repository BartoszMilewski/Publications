
postCompWith :: (a -> b) -> (x -> a) -> (x -> b)
postCompWith f = \h -> f . h

preCompWith :: (a -> b) -> (b -> x) -> (a -> x)
preCompWith f = \h -> h . f

injectBool :: Bool -> Int
injectBool b = if b then 1 else 0
