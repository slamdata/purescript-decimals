module Data.Decimal
  ( Decimal()
  , decimal
  , getMantissa
  , getExponent
  , fromInt
  , prettyDecimal
  ) where

import Prelude
import Data.Array as A
import Data.Either as E
import Data.Maybe as M
import Data.Ord as O
import Data.String as S
import Control.Monad.Rec.Class as Rec

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

type DecimalR =
  { mantissa :: Int
  , exponent :: Int
  }

-- | An abstract type for exact computing with decimal numbers of finite expansion.
newtype Decimal = Decimal DecimalR


instance showDecimal :: Show Decimal where
  show (Decimal d) =
    "decimal "
      <> showInt d.mantissa
      <> " "
      <> showInt d.exponent
    where
      showInt i =
        if i > 0
        then show i
        else "(" <> show i <> ")"



instance eqDecimal :: Eq Decimal where
  eq (Decimal d1) (Decimal d2) =
    d1.mantissa == d2.mantissa
      && (d1.exponent == d2.exponent || d1.mantissa == 0)

instance ordDecimal :: Ord Decimal where
  compare (Decimal d1) (Decimal d2) =
    case compare d1.exponent d2.exponent of
      EQ -> compare d1.mantissa d2.mantissa
      o ->
        if d1.mantissa == 0 && d2.mantissa == 0
        then EQ
        else o

type Nat = Int

pow
  :: Int
  -> Nat
  -> Int
pow i p =
  Data.Int.floor $
    Math.pow
      (Data.Int.toNumber i)
      (Data.Int.toNumber p)
--pow
--  :: Int
--  -> Nat
--  -> Int
--pow i p =
--  Rec.tailRec go { acc: 1, pwr : p }
--  where
--    go { acc, pwr } =
--      Debug.Trace.trace ("go " <> show pwr) \_ ->
--      case pwr of
--        0 -> E.Right acc
--        _ -> E.Left { acc: acc * i, pwr : pwr - 1 }


data SignedView a
  = Pos a
  | Neg a
  | Zero

signedView
  :: Int
  -> SignedView Nat
signedView 0 =
  Zero
signedView n =
  if n > 0
  then Pos n
  else Neg (- n)

prettyDecimal
  :: Decimal
  -> String
prettyDecimal (Decimal d) =
  case d.mantissa of
    0 -> "0"
    _ ->
      case signedView d.exponent of
        Pos e -> show $ d.mantissa * pow 10 d.exponent
        Neg e ->
          let
            m = show d.mantissa
            m' = M.fromMaybe m $ S.stripPrefix "-" $ m
            l = if d.mantissa > 0 then S.length m else S.length m' - 1
            zeroes = S.fromCharArray $ A.replicate (O.max 0 (e - l)) '0'
            arr = S.toCharArray $ zeroes <> m'
            i = A.length arr - e
            arr' = M.fromMaybe [] $ A.insertAt i '.' arr
            m'' = S.fromCharArray arr'
            sign = if d.mantissa > 0 then "" else "-"
          in
            sign <> if i == 0 then "0" <> m'' else m''
        Zero -> show d.mantissa

normalize
  :: DecimalR
  -> Decimal
normalize =
  Rec.tailRec \d ->
    if d.mantissa /= 0 && d.mantissa `mod` 10 == 0
    then E.Left { mantissa : div d.mantissa 10, exponent : d.exponent + 1 }
    else E.Right $ Decimal d

instance arbitraryDecimal :: SC.Arbitrary Decimal where
  arbitrary = do
    mantissa <- Gen.chooseInt (-100.0) 100.0
    exponent <- Gen.chooseInt (-20.0) 20.0
    pure $ normalize { mantissa, exponent }

instance semiringDecimal :: Semiring Decimal where
  one =
    Decimal
      { mantissa : one
      , exponent : zero
      }

  zero =
    Decimal
      { mantissa : zero
      , exponent : zero
      }

  add d1 d2 =
    normalize
      { mantissa : r.n1 + r.n2
      , exponent : r.e
      }
    where
      r = roundMax d1 d2

  mul (Decimal d1) (Decimal d2) =
    normalize
      { mantissa : d1.mantissa * d2.mantissa
      , exponent : d1.exponent + d2.exponent
      }

-- is this right?
divRound :: Int -> Int -> Int
divRound n1 n2 =
  if abs r * 2 >= abs n2
  then n + signum n1
  else n
  where
    signum i =
      if i >= 0
      then 1
      else -1
    abs i =
      if i >= 0
      then i
      else -i
    n = n1 `div` n2
    r = n1 `mod` n2

roundTo
  :: Int
  -> DecimalR
  -> DecimalR
roundTo exponent d = { mantissa , exponent }
  where
    mantissa =
      case compare exponent d.exponent of
        LT -> d.mantissa `divRound` divisor
        EQ -> d.mantissa
        GT -> d.mantissa * multiplier
    divisor =
      pow 10 (d.exponent - exponent)
    multiplier =
      pow 10 (exponent - d.exponent)

roundMax
  :: Decimal
  -> Decimal
  -> { e :: Int, n1 :: Int, n2 :: Int }
roundMax (Decimal d1) (Decimal d2) = { e, n1, n2 }
  where
    e = O.max d1.exponent d2.exponent
    d1' = roundTo e d1
    d2' = roundTo e d2
    n1 = d1'.mantissa
    n2 = d2'.mantissa

hole :: forall a. a
hole = Unsafe.Coerce.unsafeCoerce "hole"

negateDecimal
  :: Decimal
  -> Decimal
negateDecimal (Decimal d) =
  Decimal $ d { mantissa = - d.mantissa }

instance ringDecimal :: Ring Decimal where
  sub d1 d2 =
    d1 + negateDecimal d2

-- | Construct a decimal number from a mantissa and an exponent.
-- | For example, `decimal 34 96` becomes `34e96` and `decimal 20 1`
-- | becomes `2e2`.
decimal
  :: Int
  -> Int
  -> Decimal
decimal mantissa exponent =
  normalize
    { mantissa
    , exponent
    }

-- | Get the mantissa (or significand) from a decimal number.
getMantissa
  :: Decimal
  -> Int
getMantissa (Decimal d) =
  d.mantissa


-- | Get the exponent from a decimal number.
getExponent
  :: Decimal
  -> Int
getExponent (Decimal d) =
  d.exponent

-- | The integers embed into the decimals.
fromInt
  :: Int
  -> Decimal
fromInt i =
  normalize
    { mantissa : i
    , exponent : 0
    }


