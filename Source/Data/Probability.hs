module Data.Chance 
( Chance(..)
, chanceToRational
, getPercentage
, reduce
, getInverse
) where

import Data.Ratio as Ratio
import Data.Function (on)
import Data.Monoid ((<>))


-- | Represents the chance of something happening. Shouldn't get higher than 1.
data Chance = Int :/: Int
    deriving (Show)


instance Eq Chance where
    (==) = equals `on` reduce
        where equals (a :/: b) (c :/: d) = a == c && b == d


instance Ord Chance where
    compare = compare2 `on` reduce
        where compare2 (a :/: b) (c :/: d) = compare x y
                  where x = a * d
                        y = c * b


-- | Converts Chance to Rational number.
chanceToRational :: Chance -> Rational
chanceToRational (x :/: y) = toInteger x Ratio.% toInteger y


-- | Converts Chance to a percentage.
getPercentage :: Chance -> Double
getPercentage (x :/: y) = 100 * fromIntegral x / fromIntegral y


-- | Reduces the Chance's fraction to its irreducible form.
-- 
-- Especially useful to do before comparing two fractions or before printing them.
reduce :: Chance -> Chance
reduce (x :/: y) = x' :/: y'
    where commonDivisor = gcd x y
          x' = x `div` commonDivisor
          y' = y `div` commonDivisor


-- | Returns inverse chance to the given one.
--
-- In other words, if the original Chance was reflecting the chance that P happens, then the inverse one reflects the chance of P not happening.
getInverse :: Chance -> Chance
getInverse (x :/: y) = (y - x) :/: y

