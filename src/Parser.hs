{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative        ((<$>))
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import Data.Maybe (listToMaybe)
import Text.XML.Cursor (Cursor, attributeIs, attribute, content,
  element, child, descendant, ($//), (&|), (&//), (>=>), (&/), ($/))
import System.FilePath.Posix (takeBaseName)
import Data.Time.Clock (UTCTime)

import Types



extractContent = T.concat . content

name :: Cursor -> Text
name cursor = head $ cursor $// axis &| extractContent
  where
    axis = element "span"
      >=> attributeIs "class" "full-name"
      >=> child

photoUrl :: Cursor -> Text
photoUrl cursor = T.concat . concat $ cursor $// axis &| attribute "src"
  where
    axis =
          element "div" >=> attributeIs "class" "profile-picture"
      &// element "img"


currentTitle :: Cursor -> Text
currentTitle cursor = T.concat . concat $ cursor $// axis &| content
  where
    axis = element "p"
      >=> attributeIs "class" "title"
      >=> child

currentLocation :: Cursor -> Text
currentLocation cursor = T.concat . concat $ cursor $// axis &| content
  where
    axis = element "div"
      >=> attributeIs "id" "location"
      &// element "a"
      >=> child

summary :: Cursor -> Text
summary cursor = T.concat (cursor $// axis &| extractContent)
  where
    axis = element "div" >=> attributeIs "id" "summary-item-view"
      &/ element "div"
      &/ element "p"
      >=> child

experiences :: Cursor -> [Experience]
experiences cursor = cursor $// collectionAxis &| experience
  where
    collectionAxis =
          element "div" >=> attributeIs "id" "background-experience"
      &/  element "div" -- >=> attributeIs "class" "editable-item section-item current-position"
      >=> child

    titleAxis     = element "h4" &/ element "a" >=> child
    companyAxis   = element "h5" &// element "a" >=> child
    spanAxis      = element "span" >=> attributeIs "class" "experience-date-locale" >=> descendant
    localityAxis  = element "span" >=> attributeIs "class" "experience-date-locale"
      &/  element "span" >=> attributeIs "class" "locality"
      >=> child
    summaryAxis   = element "p" >=> child

    experience cursor = Experience {
        exTitle    = T.concat $ cursor $// titleAxis &| extractContent
      , exCompany  = Company $ T.concat $ cursor $// companyAxis &| extractContent
      , exSpan     = T.concat $ cursor $// spanAxis &| extractContent
      , exLocality = T.concat $ cursor $// localityAxis &| extractContent
      , exSummary  = T.concat $ cursor $// summaryAxis &| extractContent
      }

skills :: Cursor -> [Skill]
skills cursor = cursor $// skillsAxis &| skill
  where
    skillsAxis =
          element "ul" >=> attributeIs "class" "skills-section"
      >=> child

    skill :: Cursor -> Skill
    skill c = Skill {
        sName             = T.concat $ attribute "data-endorsed-item-name" c
      , sMiniEndorsements = c $// miniEndorsementsAxis &| miniEndorsement
      }

      where
        miniEndorsementsAxis =
              element "div"
          &/  element "ul" >=> attributeIs "class" "endorsers-pics"
          &/  element "li"
          &/  element "span"

        miniEndorsement c = MiniEndorsement {
            meLink     = T.concat $ attribute "data-li-url" c
          , mePhotoId  = photoId endorsementPhotoUrl
          , mePhotoUrl = endorsementPhotoUrl
          }
          where
            endorsementPhotoUrl = T.concat . concat $ c $// child &| attribute "src"


expandedSkill :: Cursor -> ExpandedSkill
expandedSkill cursor = ExpandedSkill {
    esName         = T.concat $ cursor $// expandedSkillNameAxis &| extractContent
  , esEndorsements = cursor $// endorsementsAxis &| endorsement
  }
  where
    expandedSkillNameAxis =
          element "div" >=> attributeIs "class" "endorsers-dialog-content"
      &/  element "div" >=> attributeIs "class" "subtitle"
      &/  element "span" >=> attributeIs "class" "skill-name"
      >=> child

    endorsementsAxis =
          element "ul" >=> attributeIs "class" "endorsers-list "
      &/  element "li"
      &/  element "div" >=> attributeIs "class" "endorser-info"

    endorsement c = Endorsement {
        enName     = T.concat $ c $// endorserNameAxis &| extractContent
      , enTitle    = T.concat $ c $// endorserTitleAxis &| extractContent
      , enLocation = T.concat $ c $// endorserLocationAxis &| extractContent
      }
      where
        endorserNameAxis =
              element "h3"
          &/  element "a"
          >=> child

        endorserTitleAxis =
              element "p" >=> attributeIs "class" "endorser-headline"
          >=> child

        endorserLocationAxis =
              element "p" >=> attributeIs "class" "endorser-location"
          >=> child


photoId = T.pack . takeBaseName . T.unpack

getPersonProfile
  :: Cursor
  -> UTCTime
  -> Person
getPersonProfile c ts = Person {
      pName           = name c
    , pPhotoId        = photoId $ photoUrl c
    , pPhotoUrl       = photoUrl c
    , pCurrentTitle   = currentTitle c
    , pCurrentLocation = currentLocation c
    , pSummary        = summary c
    , pExperiences    = experiences c
    , pSkills         = skills c
    , pExpandedSkill  = expandedSkill c

    , pSyncTimestamp  = ts
    }

