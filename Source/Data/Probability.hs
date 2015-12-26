module Data.Probability 
( Probability(..)
, probabilityToRational
, getPercentage
, reduce
, getInverse
, (&%&)
, (|%|)
) where

import Data.Ratio as Ratio
import Data.Function (on)


-- | Represents the probability of something happening. Shouldn't get higher than 1.
data Probability = Int :/: Int
    deriving (Show)


infix 5 :/:


instance Eq Probability where
    (==) = equals `on` reduce
        where equals (a :/: b) (c :/: d) = a == c && b == d


instance Ord Probability where
    compare = compare2 `on` reduce
        where compare2 (a :/: b) (c :/: d) = compare x y
                  where x = a * d
                        y = c * b


instance Num Probability where
    (a :/: b) + (c :/: d) = (a*d + b*c) :/: (b * d)
    
    (a :/: b) * (c :/: d) = (a * c) :/: (b * d)

    negate (a :/: b) = negate a :/: b
    
    abs (a :/: b) = abs a :/: b
    
    signum (a :/: _) = signum a :/: 1
    
    fromInteger i = fromInteger i :/: 1


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
-- Equivalent to P(A^c).
getInverse :: Probability -> Probability
getInverse (x :/: y) = y - x :/: y


-- | Returns the Probability of the two probabilities happening at the same time.
--
-- Equivalent to P(A n B), assuming that A and B are independant.
(&%&) :: Probability -> Probability -> Probability
(&%&) = (*)


-- | Returns the Probability of at least one of the two probabilities happening
--
-- Eqivalent to P(A u B), assuming that A and B are mutually exclusive.
(|%|) :: Probability -> Probability -> Probability
(|%|) = (+)

