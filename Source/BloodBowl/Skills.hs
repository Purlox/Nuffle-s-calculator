module BloodBowl.Skills
( Skill(..)
, SkillCategory(..)

, getCategory
, skills
, skillsOfCategory

, genSkills
, strSkills
, agiSkills
, pasSkills
, mutSkills
, extSkills
) where


-- | Type representing the possible Skills players could acquire in the game
data Skill
    = Accurate
    | AlwaysHungry
    | Animosity
    | BallAndChain
    | BigHand
    | Block
    | BloodLust
    | Bombardier
    | BoneHead
    | BreakTackle
    | Catch
    | Chainsaw
    | Claws
    | Dauntless
    | Decay
    | DirtyPlayer
    | DisturbingPresence
    | DivingCatch
    | DivingTackle
    | Dodge
    | DumpOff
    | ExtraArms
    | FanFavourite
    | Fend
    | FoulAppearance
    | Frenzy
    | Grab
    | Guard
    | HailMaryPass
    | Horns
    | HypnoticGaze
    | Juggernaut
    | JumpUp
    | Kick
    | KickOffReturn
    | Leader
    | Leap
    | Loner
    | MightyBlow
    | MultipleBlock
    | NervesOfSteel
    | NoHands
    | NurglesRot
    | Pass
    | PassBlock
    | PilingOn
    | PrehensileTail
    | Pro
    | ReallyStupid
    | Regeneration
    | RightStuff
    | SafeThrow
    | SecretWeapon
    | Shadowing
    | SideStep
    | SneakyGit
    | Sprint
    | Stab
    | Stakes
    | StandFirm
    | StripBall
    | StrongArm
    | Stunty
    | SureFeet
    | SureHands
    | Tackle
    | TakeRoot
    | Tentacles
    | ThickSkull
    | ThrowTeamMate
    | Titchy
    | TwoHeads
    | VeryLongLegs
    | WildAnimal
    | Wrestle
    deriving (Show, Read, Eq, Enum, Bounded)


-- | Type representing the categories of Skills
data SkillCategory
    = General
    | Strength
    | Agility
    | Passing
    | Mutation
    | Extraordinary
    deriving (Show, Read, Eq)


-- | Returns the category of a particular Skill
getCategory :: Skill -> SkillCategory
getCategory Accurate             = Passing
getCategory AlwaysHungry         = Extraordinary
getCategory Animosity            = Extraordinary
getCategory BallAndChain         = Extraordinary
getCategory BigHand              = Mutation
getCategory Block                = General
getCategory BloodLust            = Extraordinary
getCategory Bombardier           = Extraordinary
getCategory BoneHead             = Extraordinary
getCategory BreakTackle          = Strength
getCategory Catch                = Agility
getCategory Chainsaw             = Extraordinary
getCategory Claws                = Mutation
getCategory Dauntless            = General
getCategory Decay                = Extraordinary
getCategory DirtyPlayer          = General
getCategory DisturbingPresence   = Mutation
getCategory DivingCatch          = Agility
getCategory DivingTackle         = Agility
getCategory Dodge                = Agility
getCategory DumpOff              = Passing
getCategory ExtraArms            = Mutation
getCategory FanFavourite         = Extraordinary
getCategory Fend                 = General
getCategory FoulAppearance       = Mutation
getCategory Frenzy               = General
getCategory Grab                 = Strength
getCategory Guard                = Strength
getCategory HailMaryPass         = Passing
getCategory Horns                = Mutation
getCategory HypnoticGaze         = Extraordinary
getCategory Juggernaut           = Strength
getCategory JumpUp               = Agility
getCategory Kick                 = General
getCategory KickOffReturn        = General
getCategory Leader               = Passing
getCategory Leap                 = Agility
getCategory Loner                = Extraordinary
getCategory MightyBlow           = Strength
getCategory MultipleBlock        = Strength
getCategory NervesOfSteel        = Passing
getCategory NoHands              = Extraordinary
getCategory NurglesRot           = Extraordinary
getCategory Pass                 = Passing
getCategory PassBlock            = General
getCategory PilingOn             = Strength
getCategory PrehensileTail       = Mutation
getCategory Pro                  = General
getCategory ReallyStupid         = Extraordinary
getCategory Regeneration         = Extraordinary
getCategory RightStuff           = Extraordinary
getCategory SafeThrow            = Passing
getCategory SecretWeapon         = Extraordinary
getCategory Shadowing            = General
getCategory SideStep             = Agility
getCategory SneakyGit            = Agility
getCategory Sprint               = Agility
getCategory Stab                 = Extraordinary
getCategory Stakes               = Extraordinary
getCategory StandFirm            = Strength
getCategory StripBall            = General
getCategory StrongArm            = Strength
getCategory Stunty               = Extraordinary
getCategory SureFeet             = Agility
getCategory SureHands            = General
getCategory Tackle               = General
getCategory TakeRoot             = Extraordinary
getCategory Tentacles            = Mutation
getCategory ThickSkull           = Strength
getCategory ThrowTeamMate        = Extraordinary
getCategory Titchy               = Extraordinary
getCategory TwoHeads             = Mutation
getCategory VeryLongLegs         = Mutation
getCategory WildAnimal           = Extraordinary
getCategory Wrestle              = General


-- | All possible Skills
skills :: [Skill]
skills = [minBound .. maxBound]


-- | All possible Skills of a particular category
skillsOfCategory :: SkillCategory -> [Skill]
skillsOfCategory cat = filter ((== cat) . getCategory) skills


-- | All General Skills
genSkills :: [Skill]
genSkills = skillsOfCategory General

-- | All Strength Skills
strSkills :: [Skill]
strSkills = skillsOfCategory Strength

-- | All Agility Skills
agiSkills :: [Skill]
agiSkills = skillsOfCategory Agility

-- | All Passing Skills
pasSkills :: [Skill]
pasSkills = skillsOfCategory Passing

-- | All Mutation Skills
mutSkills :: [Skill]
mutSkills = skillsOfCategory Mutation

-- | All Extraordinary Skills
extSkills :: [Skill]
extSkills = skillsOfCategory Extraordinary
