-- |This module contains typeclass instance declarations for various types found
-- throughout the library. Primarily, these are instances of 'Arbitrary', from
-- "Test.QuickCheck".
-- 
-- NB: we don't create an 'Arbitrary' instance for 'Pledge's; these depend on
-- the id-numbers of the Patrons and Projects.
module Instances where

import SdMech

import Test.QuickCheck

instance Arbitrary Funds where
  arbitrary = fmap Funds arbitrary
  
instance Arbitrary Nat where
  arbitrary = fmap Nat arbitrary
