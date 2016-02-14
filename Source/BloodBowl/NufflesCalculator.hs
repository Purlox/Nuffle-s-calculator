module BloodBowl.NufflesCalculator
( probabilityOf
, probabilityOfFailing
, probabilityOfAll

, rollAtLeast
, rollAtLeastOn

, combinations

, d6
, nD6Combs
) where

import Data.Probability
import BloodBowl.Stats
import BloodBowl.Skills
import BloodBowl.Player
import BloodBowl.Rolls

import Data.List (sortOn)


-- | Returns the probability of succeeding a certain roll
probabilityOf :: DiceRoll -> Player -> Probability
probabilityOf (AgilityRoll mod) player
    = rollAtLeast $ 7 - getAG player - mod
probabilityOf (DoubleDiceRoll mod atLeast) player
    = rollAtLeastOn 2 (atLeast - mod)
probabilityOf (HypnoticGazeRoll enemies) player
    = probabilityOf (AgilityRoll (negate $ numOfTZs enemies)) player
probabilityOf JumpUpRoll player
    = probabilityOf (AgilityRoll 2) player
probabilityOf (LeapRoll maybeEnemy) player
    = probabilityOf (AgilityRoll mod) player
        *
      case maybeEnemy of
        Nothing    -> 1
        Just enemy -> probabilityOf (TentaclesRoll enemy) player
    where mod = if player `hasSkill` VeryLongLegs then 1 else 0
probabilityOf (LandingRoll enemies) player
    = probabilityOf (AgilityRoll (negate $ numOfTZs enemies)) player
probabilityOf (DodgeRoll enemiesBefore enemiesAfter) player
    = (if hasActiveDodge then withReroll else withoutReroll)
      (probabilityOf (AgilityRoll mod) player'  
        *
      if enemiesAfter `anyHasSkill` Tentacles then probabilityOf (TentaclesRoll tentacleEnemy) player else 1)
    where meaningfullTZs = filter (not . (`hasSkill` Titchy)) enemiesAfter
          modTZ = negate $ if player `hasSkill` Stunty then 0 else numOfTZs meaningfullTZs
          titchyBonus = if player `hasSkill` Titchy then 1 else 0
          tailNegBonus = if enemiesBefore `anyHasSkill` PrehensileTail then (-1) else 0
          mod = modTZ + tailNegBonus + titchyBonus + 1
          hasActiveDodge = if enemiesBefore `anyHasSkill` Tackle
                             then False
                             else player `hasSkill` Dodge
          player' = if player `hasSkill` BreakTackle
                      then player `withAG` (toAG $ getST player)
                      else player
          tentacleEnemy = head . sortOn playerST . filter (`hasSkill` Tentacles) $ enemiesBefore
probabilityOf (TentaclesRoll enemy) player
    = probabilityOf (DoubleDiceRoll st_diff 6) player
    where st_diff = getST player - getST enemy


-- | Returns the probability of failing a certain roll
probabilityOfFailing :: DiceRoll -> Player -> Probability
probabilityOfFailing dice prob = getInverse $ probabilityOf dice prob


-- | Returns the probability of succeeding all the rolls
probabilityOfAll :: [(DiceRoll, Player)] -> Probability
probabilityOfAll = getInverse . product . map (uncurry probabilityOfFailing)


-- | Returns the probability of succeeding if a reroll can be used
withReroll :: Probability -> Probability
withReroll p = getInverse $ negP * negP
    where negP = getInverse p


-- | Returns the probability of succeeding without reroll being used
withoutReroll :: Probability -> Probability
withoutReroll = id


-- | Probability that a D6 roll will succeed, when the points on the dice have to be at least as much as the argument.
-- 
-- Note that due to the rules of Blood Bowl, a critical 1 always fails and a critical 6 always succeeds. Therefore the probability is never exactly 1 or 0.
rollAtLeast :: Int -> Probability
rollAtLeast x
    | x >= 6    = 1 :/: 6
    | x <= 1    = 5 :/: 6
    | otherwise = (7-x) :/: 6


-- | Probability that a throw of N D6s will succeed, when the sum of the points on the dice has to be at least as much as the arg
rollAtLeastOn :: Int -> Int -> Probability
rollAtLeastOn n x = length (filter (>= x) possibleSums) :/: length possibleSums
    where possibleSums = map sum $ nD6Combs n


-- | Returns all combinations of the elements in the sublists
--
-- Each combination taking 1 element from a sublist and pairing it with N elements from the remaining N sublists. The combinations always have as many elements as there are sublists.
combinations :: [[a]] -> [[a]]
combinations []           = [[]]
combinations ([]:xss)     = []
combinations ((x:xs):xss) = map (x:) (combinations xss) ++ combinations (xs:xss)


-- | All possible combinations of heads of nD6
nD6Combs :: Int -> [[Int]]
nD6Combs n = combinations $ replicate n d6


-- | All possible heads of D6
d6 :: [Int]
d6 = [1..6]

