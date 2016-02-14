{-| Module containing Data representing the Stats that players can have and their possible values.

The max values of those stats are a bit on the high end to make sure that even if you got really lucky and got high stat, that you still could use the stats here.
-}
module BloodBowl.Stats
( MovementAllowance(..)
, Strength(..)
, Agility(..)
, ArmourValue(..)

, MA
, ST
, AG
, AV

, toMA
, toST
, toAG
, toAV
) where


-- | Represents the possible MA values in the game
data MovementAllowance
    = MA1
    | MA2
    | MA3
    | MA4
    | MA5
    | MA6
    | MA7
    | MA8
    | MA9
    | MA10
    | MA11
    | MA12
    deriving (Show, Read, Eq, Ord, Enum)


-- | Represents the possible ST values in the game
data Strength
    = ST1
    | ST2
    | ST3
    | ST4
    | ST5
    | ST6
    | ST7
    | ST8
    | ST9
    | ST10
    deriving (Show, Read, Eq, Ord, Enum)


-- | Represents the possible AG values in the game
data Agility
    = AG1
    | AG2
    | AG3
    | AG4
    | AG5
    | AG6
    | AG7
    | AG8
    deriving (Show, Read, Eq, Ord, Enum)


-- | Represents the possible AV values in the game
data ArmourValue
    = AV1
    | AV2
    | AV3
    | AV4
    | AV5
    | AV6
    | AV7
    | AV8
    | AV9
    | AV10
    | AV11
    | AV12
    deriving (Show, Read, Eq, Ord, Enum)


type MA = MovementAllowance
type ST = Strength
type AG = Agility
type AV = ArmourValue


-- | Converts number to MA
toMA :: Int -> MA
toMA x = toEnum (x-1)

-- | Converts number to ST
toST :: Int -> ST
toST x = toEnum (x-1)

-- | Converts number to AG
toAG :: Int -> AG
toAG x = toEnum (x-1)

-- | Converts number to AV
toAV :: Int -> AV
toAV x = toEnum (x-1)

