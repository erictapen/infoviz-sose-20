{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Codec.Compression.GZip as GZ
import Control.Monad.IO.Class
import Data.Aeson as Aeson
import Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 as BLU
import Data.ByteString.UTF8 as BSU
import Data.Maybe
import Data.Text as TS
import Data.Text.Encoding as TSE
import Data.Text.IO as TSIO
import Data.Time.LocalTime
import Numeric (showHex)
import System.Directory
import System.IO
import Prelude as P
import Data.List.Utils

import Data.Geospatial

baseUrl :: Text
baseUrl = "https://fragdenstaat.de/api/v1/"

data Vehicle
  = Vehicle
      { id :: Text,
        latitude :: Latitude,
        longitude :: Longitude
      }
  deriving (Show)

instance FromJSON Vehicle where
  parseJSON = withObject "RequestPage" $ \o -> do
    line <- o .: "line"
    id <- line .: "id"
    location <- o .: "location"
    latitude <- location .: "latitude"
    longitude <- location .: "longitude"
    return Vehicle {..}

getVehicles :: FilePath -> IO [Vehicle]
getVehicles path = do
  gzippedContent <- BL.readFile $ "./raw/" ++ path
  case (Aeson.eitherDecode $ GZ.decompress gzippedContent) of
    (Right res) -> do
      return res
    (Left err) -> do
      System.IO.hPutStrLn stderr err
      return []

getAllVehicles :: [FilePath] -> IO [[Vehicle]]
getAllVehicles [] = return []
getAllVehicles (f : fs) = do
  vehicles <- getVehicles f
  nextVehicles <- getAllVehicles fs
  return (vehicles:nextVehicles)

main :: IO ()
main = do
  fileList <- listDirectory "./raw"
  vehicles <- getAllVehicles fileList
  P.putStrLn $ join "\n" $ P.map (\v -> (show $ latitude v) ++ "," ++ (show $ longitude v)) $ P.concat vehicles

