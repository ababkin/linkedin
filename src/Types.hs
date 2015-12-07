module Types where

import Data.Text (Text)


data Company = Company {
    cName :: Text
  } deriving Show

data Experience = Experience {
    eTitle    :: Text
  , eCompany  :: Company
  , eSpan     :: Text
  , eSummary  :: Text
  } deriving Show

data Endorsement = Endorsement {
    eLink     :: Text
  , ePhotoId  :: Text
  , ePhotoUrl :: Text
  } deriving Show

data Skill = Skill {
    sName :: Text
  , sEndorsements :: [Endorsement]
  } deriving Show

data Person = Person {
    pName             :: Text
  , pPhotoId          :: Text
  , pPhotoUrl         :: Text
  , pCurrentTitle     :: Text
  , pCurrentLocation  :: Text
  , pSummary          :: Text
  , pExperiences      :: [Experience]
  , pSkills           :: [Skill]
  } deriving Show

