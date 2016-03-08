-- |Little utility functions
module SdMech.Util
       ( module SdMech.Util
         -- *** Convenience re-exports
       , module Control.Lens.TH
       , module Data.Aeson
       , module Database.Persist.TH
       , module GHC.Generics
       ) where

import Control.Lens.TH
import Data.Aeson
import Database.Persist.TH
import GHC.Generics

-- |Alias for 'mappend'
(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

-- |Alias for 'mempty'
zero :: Monoid m => m
zero = mempty
