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
-- Module      : Snowdrift.Mech.Types.Funds
-- Description : A type for funds in an account
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains operations over the 'Funds' type

module Snowdrift.Mech.Types.Funds where

import Control.Lens
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

instance Show Funds where
  show (Funds n) =  show n

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
  deriving Show

-- |Calculates the balance remaining in the account after a withdrawal
balanceAfter :: Withdrawal -> Balance
balanceAfter = \case
  GoodWithdrawal _ b -> b
  FundsEmpty _ -> mempty

-- *** Semantic aliases
type Balance = Funds
type WithdrawalAmount = Funds

-- * Lenses
makeLenses ''Funds
makeLenses ''Withdrawal
