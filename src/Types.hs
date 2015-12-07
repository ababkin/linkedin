{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (encode, ToJSON(..), genericToEncoding, defaultOptions)
import Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem, item, attr, getAttr, getAttr',
  attrAs, text, DynData(..))
import Data.Time.Clock (UTCTime)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)


data Company = Company {
    cName :: Text
  } deriving (Generic, Show)
instance ToJSON Company where
  toEncoding = genericToEncoding defaultOptions

data Experience = Experience {
    exTitle    :: Text
  , exCompany  :: Company
  , exSpan     :: Text
  , exLocality :: Text
  , exSummary  :: Text
  } deriving (Generic, Show)
instance ToJSON Experience where
  toEncoding = genericToEncoding defaultOptions

data MiniEndorsement = MiniEndorsement {
    meLink     :: Text
  , mePhotoId  :: Text
  , mePhotoUrl :: Text
  } deriving (Generic, Show)
instance ToJSON MiniEndorsement where
  toEncoding = genericToEncoding defaultOptions

data Skill = Skill {
    sName :: Text
  , sMiniEndorsements :: [MiniEndorsement]
  } deriving (Generic, Show)
instance ToJSON Skill where
  toEncoding = genericToEncoding defaultOptions

data Endorsement = Endorsement {
    enName     :: Text
  , enTitle    :: Text
  , enLocation :: Text
  } deriving (Generic, Show)
instance ToJSON Endorsement where
  toEncoding = genericToEncoding defaultOptions

data ExpandedSkill = ExpandedSkill {
    esName          :: Text
  , esEndorsements  :: [Endorsement]
  } deriving (Generic, Show)
instance ToJSON ExpandedSkill where
  toEncoding = genericToEncoding defaultOptions


data Person = Person {
    pName             :: Text
  , pPhotoId          :: Text
  , pPhotoUrl         :: Text
  , pCurrentTitle     :: Text
  , pCurrentLocation  :: Text
  , pSummary          :: Text
  , pExperiences      :: [Experience]
  , pSkills           :: [Skill]
  , pExpandedSkill    :: ExpandedSkill

  , pSyncTimestamp    :: UTCTime
  } deriving (Generic, Show)
instance ToJSON Person where
  toEncoding = genericToEncoding defaultOptions

instance ToDynItem Person where
  toItem person@Person{pName, pSyncTimestamp} =
    item [
          attr "name"           pName
        , attr "syncTimestamp"  pSyncTimestamp
        , attr "data"           (decodeUtf8 . toStrict $ encode person)
        ]
