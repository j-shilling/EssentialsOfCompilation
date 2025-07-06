{-# LANGUAGE GADTs #-}

-- | Defines the internal representation of Scheme values

module AST
  ( Expression(..)
  , Prim(..)
  , LFixnum(..)
  , LBignum(..)
  , LFlonum(..)
  , LNumber(..)
  )
where

newtype LFixnum = LFixnum Int
newtype LBignum = LBignum Integer
newtype LFlonum = LFlonum Double

toBignum :: (Integral a) => a -> LBignum
toBignum = fromInteger . toInteger

toFlonum :: (Integral a) => a -> LFlonum
toFlonum = fromInteger . toInteger

data LNumber where
  Fixnum :: LFixnum -> LNumber
  Bignum :: LBignum -> LNumber
  Flonum :: LFlonum -> LNumber

instance Eq LFixnum where
  (==) (LFixnum a) (LFixnum b) = a == b
  (/=) (LFixnum a) (LFixnum b) = a /= b

instance Eq LBignum where
  (==) (LBignum a) (LBignum b) = a == b
  (/=) (LBignum a) (LBignum b) = a /= b

instance Eq LFlonum where
  (==) (LFlonum a) (LFlonum b) = a == b
  (/=) (LFlonum a) (LFlonum b) = a /= b

instance Eq LNumber where
  (==) (Fixnum a) (Fixnum b) = a == b
  (==) (Bignum a) (Bignum b) = a == b
  (==) (Flonum a) (Flonum b) = a == b
  (==) (Fixnum a) (Bignum b) = toBignum a == b
  (==) (Fixnum a) (Flonum b) = toFlonum a == b
  (==) (Bignum a) (Fixnum b) = a == toBignum b
  (==) (Bignum a) (Flonum b) = toFlonum a == b
  (==) (Flonum a) (Fixnum b) = a == toFlonum b
  (==) (Flonum a) (Bignum b) = a == toFlonum b

instance Ord LFixnum where
  compare (LFixnum a) (LFixnum b) = compare a b
  (<) (LFixnum a) (LFixnum b) = (<) a b
  (<=) (LFixnum a) (LFixnum b) = (<=) a b
  (>) (LFixnum a) (LFixnum b) = (>) a b
  (>=) (LFixnum a) (LFixnum b) = (>=) a b
  max (LFixnum a) (LFixnum b) = LFixnum (max a b)
  min (LFixnum a) (LFixnum b) = LFixnum (min a b)

instance Ord LBignum where
  compare (LBignum a) (LBignum b) = compare a b
  (<) (LBignum a) (LBignum b) = (<) a b
  (<=) (LBignum a) (LBignum b) = (<=) a b
  (>) (LBignum a) (LBignum b) = (>) a b
  (>=) (LBignum a) (LBignum b) = (>=) a b
  max (LBignum a) (LBignum b) = LBignum (max a b)
  min (LBignum a) (LBignum b) = LBignum (min a b)

instance Ord LFlonum where
  compare (LFlonum a) (LFlonum b) = compare a b
  (<) (LFlonum a) (LFlonum b) = (<) a b
  (<=) (LFlonum a) (LFlonum b) = (<=) a b
  (>) (LFlonum a) (LFlonum b) = (>) a b
  (>=) (LFlonum a) (LFlonum b) = (>=) a b
  max (LFlonum a) (LFlonum b) = LFlonum (max a b)
  min (LFlonum a) (LFlonum b) = LFlonum (min a b)

instance Ord LNumber where
  compare (Fixnum a) (Fixnum b) = compare a  b
  compare (Bignum a) (Bignum b) = compare a  b
  compare (Flonum a) (Flonum b) = compare a  b
  compare (Fixnum a) (Bignum b) = compare (toBignum a)  b
  compare (Fixnum a) (Flonum b) = compare (toFlonum a)  b
  compare (Bignum a) (Fixnum b) = compare a  (toBignum b)
  compare (Bignum a) (Flonum b) = compare (toFlonum a)  b
  compare (Flonum a) (Fixnum b) = compare a  (toFlonum b)
  compare (Flonum a) (Bignum b) = compare a  (toFlonum b)

instance Bounded LFixnum where
  minBound = LFixnum (minBound :: Int)
  maxBound = LFixnum (maxBound :: Int)

instance Enum LFixnum where
  toEnum = LFixnum
  fromEnum (LFixnum a) = a

instance Enum LBignum where
  toEnum = LBignum . toEnum
  fromEnum (LBignum a) = fromEnum a

instance Enum LFlonum where
  toEnum = LFlonum . toEnum
  fromEnum (LFlonum a) = fromEnum a

instance Enum LNumber where
  toEnum = Fixnum . toEnum
  fromEnum (Fixnum a) = fromEnum a
  fromEnum (Bignum a) = fromEnum a
  fromEnum (Flonum a) = fromEnum a

instance Num LFixnum where
  (+) (LFixnum a) (LFixnum b) = LFixnum (a + b)
  (-) (LFixnum a) (LFixnum b) = LFixnum (a - b)
  (*) (LFixnum a) (LFixnum b) = LFixnum (a * b)
  negate (LFixnum a) = LFixnum (negate a)
  abs (LFixnum a) = LFixnum (abs a)
  signum (LFixnum a) = LFixnum (signum a)
  fromInteger = LFixnum . fromInteger

instance Num LBignum where
  (+) (LBignum a) (LBignum b) = LBignum (a + b)
  (-) (LBignum a) (LBignum b) = LBignum (a - b)
  (*) (LBignum a) (LBignum b) = LBignum (a * b)
  negate (LBignum a) = LBignum (negate a)
  abs (LBignum a) = LBignum (abs a)
  signum (LBignum a) = LBignum (signum a)
  fromInteger = LBignum . fromInteger

instance Num LFlonum where
  (+) (LFlonum a) (LFlonum b) = LFlonum (a + b)
  (-) (LFlonum a) (LFlonum b) = LFlonum (a - b)
  (*) (LFlonum a) (LFlonum b) = LFlonum (a * b)
  negate (LFlonum a) = LFlonum (negate a)
  abs (LFlonum a) = LFlonum (abs a)
  signum (LFlonum a) = LFlonum (signum a)
  fromInteger = LFlonum . fromInteger

instance Num LNumber where
  (+) (Fixnum a) (Fixnum b) = Fixnum $ a + b
  (+) (Bignum a) (Bignum b) = Bignum $ a + b
  (+) (Flonum a) (Flonum b) = Flonum $ a + b
  (+) (Fixnum a) (Bignum b) = Bignum $ toBignum a + b
  (+) (Fixnum a) (Flonum b) = Flonum $ toFlonum a + b
  (+) (Bignum a) (Fixnum b) = Bignum $ a + toBignum b
  (+) (Bignum a) (Flonum b) = Flonum $ toFlonum a + b
  (+) (Flonum a) (Fixnum b) = Flonum $ a + toFlonum b
  (+) (Flonum a) (Bignum b) = Flonum $ a + toFlonum b

  (-) (Fixnum a) (Fixnum b) = Fixnum $ a - b
  (-) (Bignum a) (Bignum b) = Bignum $ a - b
  (-) (Flonum a) (Flonum b) = Flonum $ a - b
  (-) (Fixnum a) (Bignum b) = Bignum $ toBignum a - b
  (-) (Fixnum a) (Flonum b) = Flonum $ toFlonum a - b
  (-) (Bignum a) (Fixnum b) = Bignum $ a - toBignum b
  (-) (Bignum a) (Flonum b) = Flonum $ toFlonum a - b
  (-) (Flonum a) (Fixnum b) = Flonum $ a - toFlonum b
  (-) (Flonum a) (Bignum b) = Flonum $ a - toFlonum b

  (*) (Fixnum a) (Fixnum b) = Fixnum $ a * b
  (*) (Bignum a) (Bignum b) = Bignum $ a * b
  (*) (Flonum a) (Flonum b) = Flonum $ a * b
  (*) (Fixnum a) (Bignum b) = Bignum $ toBignum a * b
  (*) (Fixnum a) (Flonum b) = Flonum $ toFlonum a * b
  (*) (Bignum a) (Fixnum b) = Bignum $ a * toBignum b
  (*) (Bignum a) (Flonum b) = Flonum $ toFlonum a * b
  (*) (Flonum a) (Fixnum b) = Flonum $ a * toFlonum b
  (*) (Flonum a) (Bignum b) = Flonum $ a * toFlonum b

  negate (Fixnum a) = Fixnum (negate a)
  negate (Bignum a) = Bignum (negate a)
  negate (Flonum a) = Flonum (negate a)

  abs (Fixnum a) = Fixnum (abs a)
  abs (Bignum a) = Bignum (abs a)
  abs (Flonum a) = Flonum (abs a)

  signum (Fixnum a) = Fixnum (signum a)
  signum (Bignum a) = Bignum (signum a)
  signum (Flonum a) = Flonum (signum a)

  fromInteger = Bignum . LBignum

instance Real LFixnum where
  toRational (LFixnum a) = toRational a

instance Real LBignum where
  toRational (LBignum a) = toRational a

instance Real LFlonum where
  toRational (LFlonum a) = toRational a

instance Integral LFixnum where
  quot (LFixnum a) (LFixnum b) = LFixnum (quot a b)
  rem (LFixnum a) (LFixnum b) = LFixnum (rem a b)
  div (LFixnum a) (LFixnum b) = LFixnum (div a b)
  mod (LFixnum a) (LFixnum b) = LFixnum (mod a b)
  quotRem (LFixnum a) (LFixnum b) = let (a', b') = quotRem a b in
    (LFixnum a', LFixnum b')
  divMod (LFixnum a) (LFixnum b) = let (a', b') = divMod a b in
    (LFixnum a', LFixnum b')
  toInteger (LFixnum a) = toInteger a

instance Integral LBignum where
  quot (LBignum a) (LBignum b) = LBignum (quot a b)
  rem (LBignum a) (LBignum b) = LBignum (rem a b)
  div (LBignum a) (LBignum b) = LBignum (div a b)
  mod (LBignum a) (LBignum b) = LBignum (mod a b)
  quotRem (LBignum a) (LBignum b) = let (a', b') = quotRem a b in
    (LBignum a', LBignum b')
  divMod (LBignum a) (LBignum b) = let (a', b') = divMod a b in
    (LBignum a', LBignum b')
  toInteger (LBignum a) = toInteger a

instance Fractional LFlonum where
  (/) (LFlonum a) (LFlonum b) = LFlonum (a / b)
  recip (LFlonum a) = LFlonum (recip a)
  fromRational = LFlonum . fromRational

instance Floating LFlonum where
  pi = LFlonum (pi :: Double)
  exp (LFlonum a) = LFlonum (exp a)
  log (LFlonum a) = LFlonum (log a)
  sqrt (LFlonum a) = LFlonum (sqrt a)
  (**) (LFlonum a) (LFlonum b) = LFlonum (a ** b)
  logBase (LFlonum a) (LFlonum b) = LFlonum (logBase a b)
  sin (LFlonum a) = LFlonum (sin a)
  cos (LFlonum a) = LFlonum (cos a)
  tan (LFlonum a) = LFlonum (tan a)
  asin (LFlonum a) = LFlonum (asin a)
  acos (LFlonum a) = LFlonum (acos a)
  atan (LFlonum a) = LFlonum (atan a)
  sinh (LFlonum a) = LFlonum (sinh a)
  cosh (LFlonum a) = LFlonum (cosh a)
  tanh (LFlonum a) = LFlonum (tanh a)
  asinh (LFlonum a) = LFlonum (asinh a)
  acosh (LFlonum a) = LFlonum (acosh a)
  atanh (LFlonum a) = LFlonum (atanh a)

instance RealFrac LFlonum where
  properFraction (LFlonum a) = let (b, a') = properFraction a in
    (b, LFlonum a')
  truncate (LFlonum a) = truncate a
  round (LFlonum a) = round a
  ceiling (LFlonum a) = ceiling a
  floor (LFlonum a) = floor a

instance RealFloat LFlonum where
  floatRadix (LFlonum a) = floatRadix a
  floatDigits (LFlonum a) = floatDigits a
  floatRange (LFlonum a) = floatRange a
  decodeFloat (LFlonum a) = decodeFloat a
  encodeFloat a b = LFlonum (encodeFloat a b)
  exponent (LFlonum a) = exponent a
  significand (LFlonum a) = LFlonum (significand a)
  scaleFloat s (LFlonum a) = LFlonum (scaleFloat s a)
  isNaN (LFlonum a) = isNaN a
  isInfinite (LFlonum a) = isInfinite a
  isDenormalized (LFlonum a) = isDenormalized a
  isNegativeZero (LFlonum a) = isNegativeZero a
  isIEEE (LFlonum a) = isIEEE a
  atan2 (LFlonum a) (LFlonum b) = LFlonum (atan2 a b)

data Prim where
  Read :: Prim
  Neg :: Expression -> Prim
  Sub :: Expression -> Expression -> Prim
  Add :: Expression -> Expression -> Prim
  deriving (Show, Eq)

data Expression where
  Prim :: Prim -> Expression
  deriving (Show, Eq)
