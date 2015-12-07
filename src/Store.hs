{-# LANGUAGE OverloadedStrings #-}

module Store where

import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Aws
import Aws.DynamoDb.Core (ToDynItem, fromItem, toItem, hk,
  UpdateReturn(URAllOld), ReturnConsumption(RCTotal),
  ReturnItemCollectionMetrics(RICMSize), toValue, Attribute(Attribute))
import Aws.DynamoDb.Commands (getItem, qrItems, scan, query, Slice(Slice), qrItems, qIndex, putItem,
  piReturn, piRetCons, piRetMet, srItems)


store
  :: ToDynItem s
  => Text
  -> s
  -> IO ()
store tableName storable = do
  let req = (putItem tableName $ toItem storable) {
      piReturn  = URAllOld
    , piRetCons = RCTotal
    , piRetMet  = RICMSize
    }

  eitherResp <- runWithCreds req
  print eitherResp
  return ()


{- runWithCreds :: Aws.AWSRequest req => req -> IO (Either String resp) -}
runWithCreds r = do
  cfg <- Aws.baseConfiguration
  maybeCreds <- Aws.loadCredentialsFromEnv
  maybe
    (return $ Left "Please set the environment variables AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET")
    (\creds -> Right <$> Aws.simpleAws cfg{Aws.credentials = creds} Aws.debugServiceConfig r)
    maybeCreds
