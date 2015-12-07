{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import qualified Text.HTML.DOM as H
import Text.XML.Cursor (fromDocument)
import           Control.Applicative        ((<$>))
import Web.Scotty

import Parser (getPersonProfile)


main :: IO ()
main = scotty 3000 $ do
  post "/linkedin/profile" $ do
    c <- fromDocument . H.parseLBS <$> body
    liftIO . print $ getPersonProfile c
    
    html "done!"



