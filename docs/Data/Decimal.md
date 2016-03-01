## Module Data.Decimal

#### `Decimal`

``` purescript
newtype Decimal
```

An abstract type for exact computing with decimal numbers of finite expansion.

##### Instances
``` purescript
Show Decimal
Eq Decimal
Ord Decimal
Arbitrary Decimal
Semiring Decimal
Ring Decimal
```

#### `prettyDecimal`

``` purescript
prettyDecimal :: Decimal -> String
```

#### `decimal`

``` purescript
decimal :: Int -> Int -> Decimal
```

Construct a decimal number from a mantissa and an exponent.
For example, `decimal 34 96` becomes `34e96` and `decimal 20 1`
becomes `2e2`.

#### `getMantissa`

``` purescript
getMantissa :: Decimal -> Int
```

Get the mantissa (or significand) from a decimal number.

#### `getExponent`

``` purescript
getExponent :: Decimal -> Int
```

Get the exponent from a decimal number.

#### `fromInt`

``` purescript
fromInt :: Int -> Decimal
```

The integers embed into the decimals.


