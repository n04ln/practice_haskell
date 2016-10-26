class Mfunctor f where
  (<$>) :: (a -> b) -> f a -> f b

instance Mfunctor [] where
  (<$>) = map

instance Mfunctor Maybe where
  f (<$>) Nothing = Nothing
  f (<$>) Just x = Just (f x)

instance Mfunctor IO where
  f (<$>) action = do
    x <- action
    return (f x)

