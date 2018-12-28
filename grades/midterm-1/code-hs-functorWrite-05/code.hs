data Pair a = Two a a
            | Zero
  deriving Show

instance Functor Pair where
   fmap f Zero = Zero
   fmap f (Two a a) = Two (f a) (f a)
