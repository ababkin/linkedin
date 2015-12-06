{-# LANGUAGE OverloadedStrings #-}

import qualified Text.HTML.DOM as H
import Text.XML.Cursor (fromDocument)
import           System.Environment         (getArgs)
import           Control.Applicative        ((<$>))

import Parser (getPersonProfile)


main :: IO ()
main = do

  [markupFile] <- getArgs
  c <- fromDocument <$> H.readFile markupFile

  print $ getPersonProfile c


