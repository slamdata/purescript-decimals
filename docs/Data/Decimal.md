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
decimal :: BigInt -> Int -> Decimal
```

Construct a decimal number from a `BigInt` mantissa and an exponent.
For example, `decimal 34 96` becomes `34e96` and `decimal 20 1`
becomes `2e2`.

#### `decimal'`

``` purescript
decimal' :: Int -> Int -> Decimal
```

The same as `decimal`, except that the mantissa is an `Int`.

#### `getMantissa`

``` purescript
getMantissa :: Decimal -> BigInt
```

Get the mantissa (or significand) from a decimal number.

#### `getExponent`

``` purescript
getExponent :: Decimal -> Int
```

Get the places from a decimal number.

#### `fromInt`

``` purescript
fromInt :: Int -> Decimal
```

The integers embed into the decimals.

#### `fromBigInt`

``` purescript
fromBigInt :: BigInt -> Decimal
```


