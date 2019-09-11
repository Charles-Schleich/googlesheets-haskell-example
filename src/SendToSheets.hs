{-# LANGUAGE OverloadedStrings #-}

module SendToSheets where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Lens hiding ( (.=))

import Data.Aeson 
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import Data.Time.LocalTime (getZonedTime,ZonedTime)
import Data.Time.Format

import Network.Google.OAuth2.JWT
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client.MultipartFormData (partBS, formDataBody)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as DBSLC

-- Types
type AuthToken = T.Text
type SheetID = T.Text
type Range = T.Text
type SheetData = [[T.Text]]
type Failure = T.Text 
type Success = T.Text 

-- Records
data GoogleAuthConfig = GoogleAuthConfig  
    { private_key_id :: T.Text
    , private_key :: T.Text
    , client_email :: T.Text
    } deriving (Show)

parseGoogleConfig :: Value -> Parser GoogleAuthConfig
parseGoogleConfig = withObject "parseGoogleConfig" $ \obj -> do 
    a <- obj .: "private_key_id"
    b <- obj .: "private_key"
    c <- obj .: "client_email"
    return $ GoogleAuthConfig a b c

-- Functions
createSignedJWT :: GoogleAuthConfig -> IO (Either String SignedJWT)
createSignedJWT googleAuth= do 
    putStrLn "Creating Signed JWT..."
    let email = client_email googleAuth
        scope = ["https://www.googleapis.com/auth/spreadsheets"]
    privatekey <- fromPEMString (T.unpack $ private_key googleAuth )
    eitherJWT <- getSignedJWT email Nothing scope (Just 600) privatekey
    return eitherJWT

getAccessToken :: GoogleAuthConfig -> IO (Either Failure (AuthToken, Manager))
getAccessToken googleAuth = do
  putStrLn "Creating Access Token..."
  eitherSignedJWT <- createSignedJWT googleAuth
  case eitherSignedJWT of
    Left err -> return $ Left $ T.pack err
    Right signedJWT -> do
      let bytestringJWT = encodeUtf8 $ T.pack $ show signedJWT
          form =  [ partBS "grant_type" "urn:ietf:params:oauth:grant-type:jwt-bearer"
                  , partBS "assertion" bytestringJWT
                  ]
      manager <- newManager tlsManagerSettings
      req <- parseRequest "https://www.googleapis.com/oauth2/v4/token"
      resp <- flip httpLbs manager =<< formDataBody form req
      let accessToken = (responseBody resp) ^? key "access_token" . _String
      case accessToken of
        Nothing -> return $ Left "Could Not Get Access Token "
        Just at -> return $ Right(at, manager)

-- Posting Data To the Server
postToSheet :: (Either Failure (AuthToken, Manager)) -> SheetID -> Range -> SheetData -> IO (Either Failure Success)
postToSheet eAccessToken sheetID range sheetData = do
  case eAccessToken of 
    Left failMessage -> return $ Left failMessage
    Right (thisAccessToken, manager) -> do
      initialReq <- parseRequest ("POST https://sheets.googleapis.com/v4/spreadsheets/" ++
                                  (T.unpack sheetID) ++
                                  "/values:batchUpdate?"++
                                  "&access_token=" ++
                                  (T.unpack thisAccessToken))
      let valueRange = object [ "range" .= (range ::T.Text)
                              , "majorDimension" .= ("ROWS" ::T.Text)
                              , "values" .= (sheetData :: [[T.Text]] )
                              ]
      let requestObject = encode $ object [ "valueInputOption" .= ("RAW"::T.Text)
                                          , "data" .= (valueRange ::Value)
                                          ]
      let request = initialReq { method = "POST", requestBody = RequestBodyLBS $ requestObject }
      response <- httpLbs request manager
      let status = responseStatus response
          statusCODE = statusCode  status  
          body = responseBody response
      putStrLn "--------\nResponse\n--------" 
      print status
      putStrLn $ DBSLC.unpack body

      case statusCODE of 
        200 -> return $ Right ( "Post Successful: " <> (T.pack $ show statusCODE ) )
        _   -> return $ Left  ( "Post Unsuccessful: " <> (T.pack $ show statusCODE ) )

-- What ends up getting sent to the server is a JSON object of the form 

-- { "valueInputOption":"RAW"
-- , "data": { "range": "Sheet1"
--           , "majorDimension": "ROWS"
--           , "values" :  [["A1","A2","A3"]
--                         ,["B1"]
--                         ,["C1","C2","C3"]
--                         ]
--           } 
-- }