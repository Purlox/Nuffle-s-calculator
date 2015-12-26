module Data.Probability 
( Probability(..)
, probabilityToRational
, getPercentage
, reduce
, getInverse
) where

import Data.Ratio as Ratio
import Data.Function (on)
import Data.Monoid ((<>))


-- | Represents the probability of something happening. Shouldn't get higher than 1.
data Probability = Int :/: Int
    deriving (Show)


instance Eq Probability where
    (==) = equals `on` reduce
        where equals (a :/: b) (c :/: d) = a == c && b == d


instance Ord Probability where
    compare = compare2 `on` reduce
        where compare2 (a :/: b) (c :/: d) = compare x y
                  where x = a * d
                        y = c * b


-- | Converts Probability to Rational number.
probabilityToRational :: Probability -> Rational
probabilityToRational (x :/: y) = toInteger x Ratio.% toInteger y


-- | Converts Probability to a percentage.
getPercentage :: Probability -> Double
getPercentage (x :/: y) = 100 * fromIntegral x / fromIntegral y


-- | Reduces the Probability's fraction to its irreducible form.
-- 
-- Especially useful to do before comparing two fractions or before printing them.
reduce :: Probability -> Probability
reduce (x :/: y) = x' :/: y'
    where commonDivisor = gcd x y
          x' = x `div` commonDivisor
          y' = y `div` commonDivisor


-- | Returns inverse Probability to the given one.
--
-- In other words, if the original Probability was of P happening, then the inverse Probability reflects P not happening.
getInverse :: Probability -> Probability
getInverse (x :/: y) = (y - x) :/: y

