data LensE s a where
    LensE :: (s -> (c, a), (c, a) -> s) -> LensE s a


toGet :: LensE s a -> (s -> a)
toGet (LensE (l, r)) = snd . l

toSet :: LensE s a -> (s -> a -> s)
toSet (LensE (l, r)) s a = r (fst (l s), a)

{- Doesn't compile
getResidue :: LensE s a -> c
getResidue (LensE (l, r)) = fst . l
-}