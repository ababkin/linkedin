{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import qualified Text.HTML.DOM as H
import Text.XML.Cursor (fromDocument)
import           Control.Applicative        ((<$>))
import Web.Scotty
import Data.Time.Clock (getCurrentTime)

import Parser (getPersonProfile)
{- import Types (Person) -}
import Store (store)


main :: IO ()
main = scotty 3000 $
  post "/linkedin/profile" $ do
    c <- fromDocument . H.parseLBS <$> body
    liftIO $ do
      ts <- getCurrentTime
      store "linkedin" $ getPersonProfile c ts

    html "done!"


