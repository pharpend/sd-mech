-- |This module contains the definition of a type, 'Funds', as well as
-- operations over said type.
module SdMech.Funds where

import SdMech.Util

import Data.Ord (comparing)
import Data.Word (Word64)

-- * Funds

-- |Funds in the mechanism are stored abstractly as natural numbers, and
-- represented as 'Word64's. The idea here is to avoid woefully imprecise
-- floating-point arithmetic.
--
-- Abstractly, instead of dollars and cents, funds are represented as a number
-- of 'mills'. A 'mill' is some fraction of a US cent.
newtype Funds = Funds { unFunds :: Word64 }
  deriving (Generic)

instance Show Funds where
  show (Funds n) = show n

instance Eq Funds where
  Funds a == Funds b = a == b

instance Ord Funds where
  compare = comparing unFunds

-- |Crucially, 'Funds' is not a group, because there are no additive inverses
-- (i.e. negative numbers). 'Funds', just being dolled-up natural numbers,
-- instead form a <https://en.wikipedia.org/wiki/Monoid#Commutative_monoid
-- commutative monoid>
--
-- Moreover, the 'Num' typeclass is a bit vague, but it's probably closest to an
-- <https://en.wikipedia.org/wiki/Ring_%28mathematics%29#Rng rng>. Nonetheless,
-- 'Num' doesn't represent any common
-- <https://en.wikipedia.org/wiki/Algebraic_structure algebraic
-- structure>.  
instance Monoid Funds where
  mempty = Funds 0
  mappend (Funds x) (Funds y) = Funds (x + y)

-- ** Withdrawals

-- |Withdraw funds; this is morally equivalent to subtraction. However, we don't
-- allow overdrawing, so if you try to withdraw more funds than are in the
-- "account", then the account is emptied, and you are given what's left.
withdraw :: Funds -> WithdrawalAmount -> Withdrawal
withdraw funds quantity
  | quantity < funds = GoodWithdrawal quantity (Funds (unFunds funds - unFunds quantity))
  | otherwise = FundsEmpty funds

-- |The result of a withdrawal
data Withdrawal = GoodWithdrawal WithdrawalAmount Balance
                | FundsEmpty WithdrawalAmount
  deriving (Show, Generic)

-- |Calculates the balance remaining in the account after a withdrawal
balanceAfter :: Withdrawal -> Balance
balanceAfter = \case
  GoodWithdrawal _ b -> b
  FundsEmpty _ -> mempty

-- *** Semantic aliases
type Balance = Funds
type WithdrawalAmount = Funds

-- ** Lens
makeLensesWith abbreviatedFields ''Funds
makePrisms ''Withdrawal

-- Aeson
instance ToJSON Funds where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Funds

instance ToJSON Withdrawal where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Withdrawal

-- Persistent
derivePersistFieldJSON "Funds"
derivePersistFieldJSON "Withdrawal"
