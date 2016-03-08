-- Snowdrift.coop - cooperative funding platform
-- Copyright (c) 2012-2016, Snowdrift.coop
-- 
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
-- 
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Snowdrift.Mech.Types.Nat
-- Description : A type for enumerable natural numbers
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX
-- 
-- 'Nat' is morally equivalent to the 'Funds' type from
-- "Snowdrift.Mech.Types.Funds". However, they are semantically different, so
-- they are represented with different types.

module Snowdrift.Mech.Types.Nat where

import Snowdrift.Mech.Util

import Control.Lens
import Data.Aeson
import Data.Ord (comparing)
import Data.Word (Word64)
import Database.Persist.TH
import GHC.Generics
import Prelude hiding (subtract)

-- * Nat

-- |Please note that this type is much different from the 'Natural' type defined
-- in "Numeric.Natural". 'Natural' is unbounded, with arbitrary
-- precision. Moreover, it uses Haskell's perverted 'Num' typeclass.
-- 
-- See "Snowdrift.Mech.Types.Funds" for an explanation of why we avoid 'Num'.
-- 
-- This type tries to be truer to the natural numbers defined by the
-- <https://en.wikipedia.org/wiki/Peano_axioms Peano axioms> (pronounced
-- "pay-ahh-no").
-- 
-- However, one of the Peano axioms is not satisfied. With 'Nat', there is a
-- number @n@ such that @'succ' n = 0@. That being the upper-bound of 'Word64'.
newtype Nat = Nat { unNat :: Word64 }
  deriving (Generic)

instance Show Nat where
  show = show . unNat

instance Eq Nat where
  (==) (Nat a) (Nat b) = (==) a b

instance Ord Nat where
  compare = comparing unNat

-- |Natural numbers actually form a commutative monoid, but the operations are
-- the same.
-- 
-- That is, for all @n, m :: Nat@, @n <> m = m <> n@.
instance Monoid Nat where
  mempty = Nat 0
  mappend (Nat n) (Nat m) = Nat (m + n)

-- |This implements subtraction, sort of. @subtract a b = a - b@. However, if @a
-- < b@, then @subtract a b = 0@.
subtract :: Nat -> Nat -> Nat
subtract m n
  | m < n = mempty
  | otherwise = Nat ((unNat m) - (unNat n))

-- |Infix form of 'subtract'
(<->) :: Nat -> Nat -> Nat
(<->) = subtract

-- |Successor of a natural number
succ :: Nat -> Nat
succ n = n <+> Nat 1

-- |Predecessor of a natural number. Note that @pred 0 = 0@.
pred :: Nat -> Nat
pred n = n <-> Nat 1


-- ** Lens
makeLensesWith abbreviatedFields ''Nat

-- Aeson
instance ToJSON Nat where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Nat

-- Persistent
derivePersistFieldJSON "Nat"
