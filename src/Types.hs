module Types where

import Data.Text (Text)


data Company = Company {
    cName :: Text
  } deriving Show

data Experience = Experience {
    eTitle    :: Text
  , eCompany  :: Company
  , eSpan     :: Text
  } deriving Show

data Endorsement = Endorsement {
    eLink     :: Text
  , ePhotoUrl :: Text
  } deriving Show

data Skill = Skill {
    sName :: Text
  , sEndorsements :: [Endorsement]
  } deriving Show

data Person = Person {
    pName           :: Text
  , pPhotoUrl       :: Text
  , pCurrentTitle   :: Text
  , pCurrentCompany :: Maybe Company
  , pSummary        :: Text
  , pExperiences    :: [Experience]
  , pSkills         :: [Skill]
  } deriving Show

