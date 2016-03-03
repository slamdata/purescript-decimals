module Data.Decimal
  ( Decimal()
  , decimal
  , decimal'
  , getMantissa
  , getExponent
  , fromInt
  , fromBigInt
  , prettyDecimal
  ) where

import Prelude
import Data.Array as A
import Data.Either as E
import Data.Maybe as M
import Data.Ord as O
import Data.String as S
import Control.Monad.Rec.Class as Rec

import Data.BigInt as BI

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

-- This representation and some of the code is based on the `Decimal` Haskell
-- package by Paul Johnson.

type DecimalR =
  { mantissa :: BI.BigInt
  , places :: Int
  }

-- | An abstract type for exact computing with decimal numbers of finite expansion.
newtype Decimal = Decimal DecimalR


instance showDecimal :: Show Decimal where
  show (Decimal d) =
    "decimal "
      <> showInt d.mantissa
      <> " "
      <> showInt d.places
    where
      showInt :: forall i. (Show i, Ring i, Ord i) => i -> String
      showInt i =
        if i > zero
        then show i
        else "(" <> show i <> ")"

instance eqDecimal :: Eq Decimal where
  eq (Decimal d1) (Decimal d2) =
    d1.mantissa == d2.mantissa
      && (d1.places == d2.places || d1.mantissa == zero)

instance ordDecimal :: Ord Decimal where
  compare (Decimal d1) (Decimal d2) =
    case compare d1.places d2.places of
      EQ -> compare d1.mantissa d2.mantissa
      o ->
        if d1.mantissa == zero && d2.mantissa == zero
        then EQ
        else o

type Nat = Int

data SignedView a
  = Pos a
  | Neg a
  | Zero

signedView
  :: Int
  -> SignedView Nat
signedView i =
  if i > zero
  then Pos i
  else if i == zero
  then Zero
  else Neg (i * -1)

prettyDecimal
  :: Decimal
  -> String
prettyDecimal (Decimal d) =
  if d.mantissa == zero
  then "0"
  else
    let
      exponent = -d.places
    in
      case signedView exponent of
        Pos e -> BI.toString $ d.mantissa * BI.pow (BI.fromInt 10) (BI.fromInt exponent)
        Neg e ->
          let
            m = BI.toString d.mantissa
            m' = M.fromMaybe m $ S.stripPrefix "-" $ m
            l = if d.mantissa > zero then S.length m else S.length m' - 1
            zeroes = S.fromCharArray $ A.replicate (O.max zero (e - l)) '0'
            arr = S.toCharArray $ zeroes <> m'
            i = A.length arr - e
            arr' = M.fromMaybe [] $ A.insertAt i '.' arr
            m'' = S.fromCharArray arr'
            sign = if d.mantissa > zero then "" else "-"
          in
            sign <> if i == 0 then "0" <> m'' else m''
        Zero -> BI.toString d.mantissa

normalize
  :: DecimalR
  -> Decimal
normalize =
  Rec.tailRec \d ->
    if d.mantissa /= zero && d.mantissa `mod` BI.fromInt 10 == zero
    then E.Left { mantissa : div d.mantissa (BI.fromInt 10), places : d.places - 1 }
    else E.Right $ Decimal d

instance arbitraryDecimal :: SC.Arbitrary Decimal where
  arbitrary = do
    mantissa <- BI.fromInt <$> Gen.chooseInt (-100.0) 100.0
    places <- Gen.chooseInt (-20.0) 20.0
    pure $ normalize { mantissa, places }

instance semiringDecimal :: Semiring Decimal where
  one =
    Decimal
      { mantissa : one
      , places : zero
      }

  zero =
    Decimal
      { mantissa : zero
      , places : zero
      }

  add d1 d2 =
    normalize
      { mantissa : r.n1 + r.n2
      , places : r.e
      }
    where
      r = roundMax d1 d2

  mul (Decimal d1) (Decimal d2) =
    normalize
      { mantissa : d1.mantissa * d2.mantissa
      , places : d1.places + d2.places
      }

divRound :: BI.BigInt -> BI.BigInt -> BI.BigInt
divRound n1 n2 =
  if abs r * BI.fromInt 2 >= abs n2
  then n + signum n1
  else n
  where
    signum i =
      BI.fromInt $
        if i >= zero
        then 1
        else -1
    abs i =
      if i >= zero
      then i
      else i * BI.fromInt (-1)
    n = n1 `div` n2
    r = n1 `mod` n2

roundTo
  :: Int
  -> DecimalR
  -> DecimalR
roundTo places d = { mantissa , places }
  where
    mantissa =
      case compare places d.places of
        LT -> d.mantissa `divRound` divisor
        EQ -> d.mantissa
        GT -> d.mantissa * multiplier
    divisor =
      BI.pow (BI.fromInt 10) (BI.fromInt $ d.places - places)
    multiplier =
      BI.pow (BI.fromInt 10) (BI.fromInt $ places - d.places)

roundMax
  :: Decimal
  -> Decimal
  -> { e :: Int, n1 :: BI.BigInt, n2 :: BI.BigInt }
roundMax (Decimal d1) (Decimal d2) = { e, n1, n2 }
  where
    e = O.max d1.places d2.places
    d1' = roundTo e d1
    d2' = roundTo e d2
    n1 = d1'.mantissa
    n2 = d2'.mantissa

negateDecimal
  :: Decimal
  -> Decimal
negateDecimal (Decimal d) =
  Decimal $ d { mantissa = - d.mantissa }

instance ringDecimal :: Ring Decimal where
  sub d1 d2 =
    d1 + negateDecimal d2

-- | Construct a decimal number from a mantissa and an places.
-- | For example, `decimal 34 96` becomes `34e96` and `decimal 20 1`
-- | becomes `2e2`.
decimal
  :: BI.BigInt
  -> Int
  -> Decimal
decimal mantissa exponent =
  normalize
    { mantissa
    , places: -exponent
    }

decimal'
  :: Int
  -> Int
  -> Decimal
decimal' =
  decimal <<< BI.fromInt

-- | Get the mantissa (or significand) from a decimal number.
getMantissa
  :: Decimal
  -> BI.BigInt
getMantissa (Decimal d) =
  d.mantissa

-- | Get the places from a decimal number.
getExponent
  :: Decimal
  -> Int
getExponent (Decimal d) =
  - d.places

-- | The integers embed into the decimals.
fromInt
  :: Int
  -> Decimal
fromInt =
  fromBigInt
    <<< BI.fromInt

fromBigInt
  :: BI.BigInt
  -> Decimal
fromBigInt mantissa =
  normalize
    { mantissa
    , places : 0
    }
