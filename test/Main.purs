module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())

import Data.Decimal as D
import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC

-- | Classical/material implication for booleans
(~>) :: Boolean -> Boolean -> Boolean
(~>) p q = not p || q

infixr 3 ~>

type TestEffects e =
  ( console :: CONSOLE
  , random :: RANDOM
  , err :: EXCEPTION
  | e
  )

type Test e a = Eff (TestEffects e) a
data Proxy a = Proxy

verify :: forall e prop. (SC.Testable prop) => prop -> Test e Unit
verify = SC.quickCheck' 1000

-- | Test suite for the `Eq` type class.
testEq
  :: forall e a
   . (SC.Arbitrary a, Eq a, Show a)
   => Proxy a
   -> Test e Unit
testEq _ = do
  verify \(a :: a) ->
    a == a
      <?> "reflexivity" <> show [a]

  verify \(a :: a) b ->
    a == b ~> b == a
      <?> "symmetry" <> show [a,b]

  verify \(a :: a) b c ->
    a == b ~> b == c ~> a == c
      <?> "transitivity" <> show [a,b,c]

-- | Test suite for the `Semiring` type class
testSemiring
  :: forall e a
   . (SC.Arbitrary a, Semiring a, Eq a, Show a)
  => Proxy a
  -> Test e Unit
testSemiring _ = do
  verify \(a :: a) a' b b' ->
    (a == a') ~> (b == b') ~>
      (a + b) == (a' + b')
        <?> "add-functionality" <> show [a,a',b,b']

  verify \(a :: a) a' b b' ->
    (a == a') ~> (b == b') ~>
      (a * b) == (a' * b')
        <?> "mul-functionality" <> show [a,a',b,b']

  verify \(a :: a) b c ->
    (a + b) + c == a + (b + c)
      <?> "add-associativity" <> show [a,b,c]

  verify \(a :: a) ->
    zero + a == a
      <?> "add-identity" <> show [a]

  verify \(a :: a) b ->
    a + b == b + a
      <?> "add-commutativity" <> show [a,b]

  verify \(a :: a) b c ->
    (a * b) * c == a * (b * c)
      <?> "mul-associativity" <> show [a,b,c]

  verify \(a :: a) ->
    one * a == a && a * one == a
      <?> "mul-identity" <> show [a]

  verify \(a :: a) b c ->
    a * (b + c) == (a * b) + (a * c)
      <?> "mul-left-distributivity" <> show [a,b,c]

  verify \(a :: a) b c ->
    (a + b) * c == (a * c) + (b * c)
      <?> "mul-right-distributivity" <> show [a,b,c]

  verify \(a :: a) ->
    zero * a == zero
      <?> "mul-annihilation" <> show [a]

testCommutativeSemiring
  :: forall e a
   . (SC.Arbitrary a, Semiring a, Eq a, Show a)
  => Proxy a
  -> Test e Unit
testCommutativeSemiring _ = do
  verify \(a :: a) b ->
    a * b == b * a
      <?> "mul-commutativity" <> show [a,b]

testRing
  :: forall e a
   . (SC.Arbitrary a, Ring a, Eq a, Show a)
  => Proxy a
  -> Test e Unit
testRing _ = do
  verify \(a :: a) a' b b' ->
    (a == a') ~> (b == b') ~>
      (a - b) == (a' - b')
        <?> "sub-functionality" <> show [a,a',b,b']

  verify \(a :: a) ->
    a - a == (zero - a) + a && a - a == zero
      <?> "additive-inverse" <> show [a]

main :: forall e. Test e Unit
main = do
  let decimal = Proxy :: Proxy D.Decimal
  testEq decimal
  testSemiring decimal
  testCommutativeSemiring decimal
  testRing decimal
