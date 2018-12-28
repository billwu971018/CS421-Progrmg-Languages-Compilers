data CalculationStep a = Step a
                       | DivByZero
                       deriving Show

instance Functor CalculationStep where
  fmap f (Step a)  = Step (f a)
  fmap f DivByZero = DivByZero

instance Applicative CalculationStep where
  pure x = Step x

  (Step f) <*> (Step x) = Step (f x)
  _        <*> _        = DivByZero

instance Monad CalculationStep where
    return a = f a
    Step f >>= Step a = f a
    _ >>= _ = Bar