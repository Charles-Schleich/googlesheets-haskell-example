{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Lazy as DBSL
import System.Environment
import Data.Text
-- my imports
import SendToSheets

data Conf = Conf    { cSheetID  :: Text
                    , cRange     :: Text
                    }

loadGoogleConfig :: FilePath -> IO (Either String GoogleAuthConfig)
loadGoogleConfig fp = do 
    contents <- DBS.readFile fp
    let eGoogleConfig = parseEither parseGoogleConfig =<< eitherDecode (DBSL.fromStrict contents)
    return eGoogleConfig

loadProgConfig :: FilePath -> IO (Either String Conf)
loadProgConfig fp = do 
    contents <- DBS.readFile fp
    let eConfig = parseEither parseConfig =<< eitherDecode (DBSL.fromStrict contents)
    -- case eConfig of 
    --     Just conf -> if conf
    return eConfig

parseConfig :: Value -> Parser Conf
parseConfig = withObject "parseConfig" $ \obj -> do 
    a <- obj .: "sheetID"
    b <- obj .: "range"
    return $ Conf a b

-- Main
main :: IO ()
main = do
    eGoogleConfig <- loadGoogleConfig "config/gsheetsworker-ab230659bce7.json"
    eConfig <- loadProgConfig "config/config.json"
    case (eGoogleConfig,eConfig) of
        (Left err,_) -> error err
        (_,Left err) -> error err
        (Right googleConfig,Right config) -> do

            -- 1. create Access Token
            eAccessToken <- getAccessToken  googleConfig

            -- 2. Send Data To sheets
            let sheetID = cSheetID config
            let range   = cRange config
            let sheetData = [["Hi", "My name is"]
                            ,["What"]
                            ,["My", "Name","is"]
                            ,["Who"]
                            ,["My Name is"]
                            ,["Slicka","Slicka","Slim Shady"]
                            ]
            result <- postToSheet eAccessToken sheetID range sheetData
            print result