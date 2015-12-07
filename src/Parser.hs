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

    titleAxis   = element "h4" &/ element "a" >=> child
    companyAxis = element "h5" &// element "a" >=> child
    spanAxis    = element "span" >=> attributeIs "class" "experience-date-locale" >=> descendant
    summaryAxis = element "p" >=> child

    experience :: Cursor -> Experience
    experience cursor = Experience {
        eTitle    = T.concat $ cursor $// titleAxis &| extractContent
      , eCompany  = Company $ T.concat $ cursor $// companyAxis &| extractContent
      , eSpan     = T.concat $ cursor $// spanAxis &| extractContent
      , eSummary  = T.concat $ cursor $// summaryAxis &| extractContent
      }

skills :: Cursor -> [Skill]
skills cursor = cursor $// skillsAxis &| skill
  where
    skillsAxis =
          element "ul" >=> attributeIs "class" "skills-section"
      >=> child

    endorsementsAxis = element "div"
      &/ element "ul" >=> attributeIs "class" "endorsers-pics" 
      &/ element "li"
      &/ element "span"

    skill :: Cursor -> Skill
    skill c = Skill {
        sName         = T.concat $ attribute "data-endorsed-item-name" c
      , sEndorsements = c $// endorsementsAxis &| endorsement
      }

    endorsement :: Cursor -> Endorsement
    endorsement c = Endorsement {
        eLink     = T.concat $ attribute "data-li-url" c
      , ePhotoId  = photoId endorsementPhotoUrl
      , ePhotoUrl = endorsementPhotoUrl
      }
      where
        endorsementPhotoUrl = T.concat . concat $ c $// child &| attribute "src"


photoId = T.pack . takeBaseName . T.unpack

getPersonProfile :: Cursor -> Person
getPersonProfile c = Person {
      pName           = name c
    , pPhotoId        = photoId $ photoUrl c
    , pPhotoUrl       = photoUrl c
    , pCurrentTitle   = currentTitle c
    , pCurrentLocation = currentLocation c
    , pSummary        = summary c
    , pExperiences    = experiences c
    , pSkills         = skills c
    }

