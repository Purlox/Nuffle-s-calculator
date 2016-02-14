module BloodBowl.Rolls
( DiceRoll(..)
) where


import BloodBowl.Player


-- | Type representing actions that require a roll in Blood Bowl.
data DiceRoll = AgilityRoll { modifier :: Int }
              | DoubleDiceRoll { modifier :: Int
                              , atLeast :: Int
                              }
              | HypnoticGazeRoll { enemiesAround :: [Player] }
              | JumpUpRoll
              | LeapRoll { maybePlayerWithTentacles :: Maybe Player }
              | LandingRoll { enemiesAround :: [Player] }
              | DodgeRoll { enemiesBeforeMove :: [Player]
                          , enemiesAfterMove :: [Player]
                          }
              
              | TentaclesRoll { playerWithTentacles :: Player }
