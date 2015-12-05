{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative        ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           System.Environment         (getArgs)
import Data.Maybe (listToMaybe)
import qualified Text.HTML.DOM as H
import Text.XML.Cursor (Cursor, attributeIs, attribute, content, laxAttribute,
  element, fromDocument, child, descendant,
  ($//), (&|), (&//), (>=>), (&/), ($/))



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

currentCompany :: Cursor -> Maybe Company
currentCompany cursor = Company <$> listToMaybe (cursor $// axis &| extractContent)
  where
    axis = element "p" >=> attributeIs "class" "title"
      &/ element "strong" >=> attributeIs "class" "highlight"
      >=> child

summary :: Cursor -> Text
summary cursor = T.concat (cursor $// axis &| extractContent)
  where
    axis = element "div" >=> attributeIs "id" "summary-item-view"
      &/ element "div"
      &/ element "p"
      >=> child

experiences :: Cursor -> [Experience]
experiences cursor = cursor $// collectionAxis &| extractExperience
  where
    collectionAxis = 
          element "div" >=> attributeIs "id" "background-experience"
      &/  element "div" -- >=> attributeIs "class" "editable-item section-item current-position"
      >=> child

    titleAxis   = element "h4" &/ element "a" >=> child
    companyAxis = element "h5" &// element "a" >=> child
    spanAxis    = element "span" >=> attributeIs "class" "experience-date-locale" >=> descendant

    extractExperience :: Cursor -> Experience
    extractExperience cursor = Experience {
        eTitle    = T.concat $ cursor $// titleAxis &| extractContent
      , eCompany  = Company $ T.concat $ cursor $// companyAxis &| extractContent
      , eSpan     = T.concat $ cursor $// spanAxis &| extractContent
      }

skills :: Cursor -> [Skill]
skills cursor = cursor $// skillsAxis &| extractSkill
  where
    skillsAxis = 
          element "ul" >=> attributeIs "class" "skills-section"
      >=> child

    endorsementsAxis = element "div"
      &/ element "ul" >=> attributeIs "class" "endorsers-pics" 
      &/ element "li"
      &/ element "span"

    extractSkill :: Cursor -> Skill
    extractSkill c = Skill {
        sName         = T.concat $ attribute "data-endorsed-item-name" c
      , sEndorsements = c $// endorsementsAxis &| extractEndorsement
      }

    extractEndorsement :: Cursor -> Endorsement
    extractEndorsement c = Endorsement {
        eLink     = T.concat $ attribute "data-li-url" c
      , ePhotoUrl = T.concat . concat $ c $// child &| attribute "src"
      }




main :: IO ()
main = do

  [markupFile] <- getArgs
  c <- fromDocument <$> H.readFile markupFile

  print $ Person {
      pName           = name c
    , pPhotoUrl       = photoUrl c
    , pCurrentTitle   = currentTitle c
    , pCurrentCompany = currentCompany c
    , pSummary        = summary c
    , pExperiences    = experiences c
    , pSkills         = skills c
    }


