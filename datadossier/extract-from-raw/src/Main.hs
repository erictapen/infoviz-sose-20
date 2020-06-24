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
import Data.Geospatial
import Data.List.Utils
import Data.Maybe
import Data.Text as TS
import Data.Text.Encoding as TSE
import Data.Text.IO as TSIO
import Data.Time.LocalTime
import Numeric (showHex)
import System.Directory
import System.IO
import Prelude as P

basePath :: FilePath
basePath = "./raw/2020-06-24/"

data Vehicle
  = Vehicle
      { id :: Text,
        latitude :: Latitude,
        longitude :: Longitude,
        trip :: Int
      }
  deriving (Show)

instance FromJSON Vehicle where
  parseJSON = withObject "Vehicle" $ \o -> do
    line <- o .: "line"
    trip <- o .: "trip"
    id <- line .: "id"
    location <- o .: "location"
    latitude <- location .: "latitude"
    longitude <- location .: "longitude"
    return Vehicle {..}

type Filter = Vehicle -> Bool

getVehicles :: FilePath -> IO [Vehicle]
getVehicles path = do
  gzippedContent <- BL.readFile $ basePath ++ path
  case (Aeson.eitherDecode $ GZ.decompress gzippedContent) of
    (Right res) -> do
      return res
    (Left err) -> do
      System.IO.hPutStrLn stderr err
      return []

getAllVehicles :: [FilePath] -> Filter -> IO [Vehicle]
getAllVehicles [] _ = return []
getAllVehicles (f : fs) vehicleFilter = do
  vehicles <- getVehicles f
  nextVehicles <- getAllVehicles fs vehicleFilter
  return $ (P.filter vehicleFilter vehicles) ++ nextVehicles

main :: IO ()
main = do
  fileList <- listDirectory basePath
  vehicles <- getAllVehicles fileList (\v -> trip v == 53928)
  P.putStrLn
    $ join "\n"
    $ P.map (\v -> (show $ latitude v) ++ "," ++ (show $ longitude v)) vehicles
