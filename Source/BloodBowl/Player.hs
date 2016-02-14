module BloodBowl.Player
( Player(..)
, PlayerState(..)

, getMA
, getST
, getAG
, getAV
, getSkills
, getState

, hasSkill
, anyHasSkill

, withMA
, withST
, withAG
, withAV
, withSkills
, withState

, isUp
, isProne
, isStunned
, isUseless

, hasTackleZone
, numOfTZs
) where

import BloodBowl.Stats
import BloodBowl.Skills


-- | Type representing a player on the field
data Player = Player { playerMA :: MA
                     , playerST :: ST
                     , playerAG :: AG
                     , playerAV :: AV
                     , playerSkills :: [Skill]
                     , playerState :: PlayerState
                     } deriving (Eq, Show)


data PlayerState = Up
                 | Prone
                 | Stunned
                 | Useless -- ^ The state player has if the player is under effects of Bone-Head, Really Stupid or Hypnotic gaze
                 deriving (Eq, Show)


-- | Returns Player's MA as an Int for easier time working with it
getMA :: Player -> Int
getMA = (+1) . fromEnum . playerMA

-- | Returns Player's ST as an Int for easier time working with it
getST :: Player -> Int
getST = (+1) . fromEnum . playerST

-- | Returns Player's AG as an Int for easier time working with it
getAG :: Player -> Int
getAG = (+1) . fromEnum . playerAG

-- | Returns Player's AV as an Int for easier time working with it
getAV :: Player -> Int
getAV = (+1) .fromEnum . playerAV

-- | Just an alias for playerSkills
getSkills :: Player -> [Skill]
getSkills = playerSkills

-- | Just an alias for playerState
getState :: Player -> PlayerState
getState = playerState


-- | Checks if Player has a certain skill
hasSkill :: Player -> Skill -> Bool
player `hasSkill` skill = skill `elem` getSkills player 


-- | Checks if at least 1 of the players has a certain skill
anyHasSkill :: [Player] -> Skill -> Bool
players `anyHasSkill` skill = skill `elem` playersSkills
    where playersSkills = concat $ map getSkills players


-- | Replaces the MA of a player
withMA :: Player -> MA -> Player
(Player _ st ag av skills state) `withMA` ma
    = Player ma st ag av skills state

-- | Replaces the ST of a player
withST :: Player -> ST -> Player
(Player ma _ ag av skills state) `withST` st
    = Player ma st ag av skills state

-- | Replaces the AG of a player
withAG :: Player -> AG -> Player
(Player ma st _ av skills state) `withAG` ag
    = Player ma st ag av skills state

-- | Replaces the AV of a player
withAV :: Player -> AV -> Player
(Player ma st ag _ skills state) `withAV` av
    = Player ma st ag av skills state

-- | Replaces the Skills of a player
withSkills :: Player -> [Skill] -> Player
(Player ma st ag av _ state) `withSkills` skills
    = Player ma st ag av skills state

-- | Replaces the State of a player
withState :: Player -> PlayerState -> Player
(Player ma st ag av skills _) `withState` state
    = Player ma st ag av skills state


-- | Checks if Player is up
isUp :: Player -> Bool
isUp player = Up == getState player

-- | Checks if Player is prone
isProne :: Player -> Bool
isProne player = Prone == getState player

-- | Checks if Player is stunned
isStunned :: Player -> Bool
isStunned player = Stunned == getState player

-- | Checks if player is "useless" aka under effects of Bone-head, Really stupid or Hypnotic gaze
isUseless :: Player -> Bool
isUseless player = Useless == getState player


-- | Checks if the player is producing tackle zones
hasTackleZone :: Player -> Bool
hasTackleZone = isUp


-- | Returns number of Tackle Zones
numOfTZs :: [Player] -> Int
numOfTZs = length . filter hasTackleZone
