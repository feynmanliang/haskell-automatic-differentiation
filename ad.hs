{-# LANGUAGE NoMonomorphismRestriction #-}

module AD (
  Dual(..)
  , f
  , idD
) where

-- Dual numbers for encoding chain rule implicitly
data Dual = Dual Double Double
  deriving (Eq, Show)

constD :: Double -> Dual
constD x = Dual x 0

idD :: Double -> Dual
idD x = Dual x 1.0

instance Num Dual where
  fromInteger n             = constD $ fromInteger n
  (Dual x x') + (Dual y y') = Dual (x + y) (x' + y')
  (Dual x x') * (Dual y y') = Dual (x * y) (x * y' + x' * y)
  negate (Dual x x')        = Dual (negate x) (negate x')
  signum _                  = undefined
  abs _                     = undefined

instance Fractional Dual where
  fromRational p           = constD $ fromRational p
  recip (Dual x x') = Dual (1.0 / x) (-x' / (x*x))

instance Floating Dual where
  pi                = constD pi
  exp   (Dual x x') = Dual (exp x)   (x' * exp x)
  log   (Dual x x') = Dual (log x)   (x' / x)
  sqrt  (Dual x x') = Dual (sqrt x)  (x' / (2 * sqrt x))
  sin   (Dual x x') = Dual (sin x)   (x' * cos x)
  cos   (Dual x x') = Dual (cos x)   (x' * (- sin x))
  sinh  (Dual x x') = Dual (sinh x)  (x' * cosh x)
  cosh  (Dual x x') = Dual (cosh x)  (x' * sinh x)
  asin  (Dual x x') = Dual (asin x)  (x' / sqrt (1 - x*x))
  acos  (Dual x x') = Dual (acos x)  (x' / (-sqrt (1 - x*x)))
  atan  (Dual x x') = Dual (atan x)  (x' / (1 + x*x))
  asinh (Dual x x') = Dual (asinh x) (x' / sqrt (1 + x*x))
  acosh (Dual x x') = Dual (acosh x) (x' / sqrt (x*x - 1))
  atanh (Dual x x') = Dual (atanh x) (x' / (1 - x*x))

f :: Floating c => c -> c
f = sqrt . (*3) . sin

-- The analytical derivative of f, `f' 2` should equal `f $ idD 2`
f' x = 3 * cos x / (2 * sqrt (3 * sin x))

-- Linear regression cost
cost :: Fractional a => a -> a -> [a] -> [a] -> a
cost a b xs ys = (/ (2 * (fromIntegral $ length xs))) $
                 sum $
                 zipWith errSq xs ys
  where
    errSq x y = z * z
      where
        z = y - (a * x + b)

-- dummy data
xs = [1,2,3,4,5,6,7]
ys = [3,5,7,9,11,13,15]

-- learning rate
alpha = 0.04

-- objective function
g :: Fractional a => a -> a -> a
g a b = cost a b xs ys

-- stream of estimates
zs = (0.1, 0.1) : map f zs
  where
    deriv (Dual _ x') = x'
    f (a, b) = (a - alpha * aDeriv, b - alpha * bDeriv)
      where
        aDeriv = deriv $ g (idD a) $ constD b
        bDeriv = deriv $ g (constD a) $ idD b

default ()
