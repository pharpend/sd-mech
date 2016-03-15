-- |Little utility functions
module SdMech.Util where

-- |Alias for 'mappend'
infixl 5 <+>
(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

-- |Alias for 'mempty'
zero :: Monoid m => m
zero = mempty
