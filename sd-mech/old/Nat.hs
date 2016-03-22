-- |This module contains a not-quite-mathematically correct 'Nat' type for
-- natural numbers: @N = {0, 1, 2, 3, ...}@.  'Nat' is morally equivalent to the
-- 'Funds' type from "SdMech.Types.Funds". However, they are
-- semantically different, so they are represented with different types.

module SdMech.Nat where

import SdMech.Util

import Data.Ord (comparing)
import Data.Word (Word64)
import Prelude hiding (subtract)

-- * Nat

-- |Please note that this type is much different from the 'Natural' type defined
-- in "Numeric.Natural". 'Natural' is unbounded, with arbitrary
-- precision. Moreover, it uses Haskell's perverted 'Num' typeclass.
-- 
-- See "SdMech.Types.Funds" for an explanation of why we avoid 'Num'.
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
