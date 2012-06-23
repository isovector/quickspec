-- | The "prelude": a standard signature containing useful functions
--   like '++', which can be used as background theory.

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Test.QuickSpec.Prelude where

import Test.QuickSpec.Signature
import Test.QuickCheck
import Data.Typeable

newtype A = A Int deriving (Eq, Ord, Typeable, Arbitrary, CoArbitrary)

data Two = One | Two deriving (Eq, Ord, Typeable)

instance Arbitrary Two where
  arbitrary = elements [One, Two]

instance CoArbitrary Two where
  coarbitrary One = variant 0
  coarbitrary Two = variant (-1)

bools :: Sig
bools = background [
  ["x", "y", "z"] `vars` (undefined :: Bool),

  "||"    `fun2` (||),
  "&&"    `fun2` (&&),
  "not"   `fun1` not,
  "True"  `fun0` True,
  "False" `fun0` False]

arith :: forall a. (Typeable a, Ord a, Num a, Arbitrary a) => a -> Sig
arith _ = background [
  ["x", "y", "z"] `vars` (undefined :: a),

  "0" `fun0` (0   :: a),
  "1" `fun0` (1   :: a),
  "+" `fun2` ((+) :: a -> a -> a),
  "*" `fun2` ((*) :: a -> a -> a)]

lists :: forall a. (Typeable a, Ord a, Arbitrary a) => a -> Sig
lists _ = background [
  ["xs", "ys", "zs"] `vars` (undefined :: [a]),

  "[]"      `fun0` ([]      :: [a]),
  ":"       `fun2` ((:)     :: a -> [a] -> [a]),
  "head"    `fun1` (head    :: [a] -> a),
  "tail"    `fun1` (tail    :: [a] -> [a]),
  "++"      `fun2` ((++)    :: [a] -> [a] -> [a]),
  "reverse" `fun1` (reverse :: [a] -> [a]),
  "length"  `fun1` (length  :: [a] -> Int)]

funs :: forall a. (Typeable a, Ord a, Arbitrary a, CoArbitrary a) => a -> Sig
funs _ = background [
  ["f", "g", "h"] `vars` (undefined :: a -> a),

  "."  `blind2` ((.) :: (a -> a) -> (a -> a) -> (a -> a)),
  "id" `blind0` (id  :: a -> a),

  observer2 (\(x :: a) (f :: a -> a) -> f x)
  ]

prelude :: (Typeable a, Ord a, Arbitrary a) => a -> Sig
prelude a = background [
  ["x", "y", "z"] `vars` a,
  bools,
  arith (undefined :: Int),
  lists a ]
