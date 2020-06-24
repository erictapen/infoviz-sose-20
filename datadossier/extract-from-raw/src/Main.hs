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
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Text as TS
import Data.Text.Encoding as TSE
import Data.Text.IO as TSIO
import Data.Text.Lazy.Encoding as TLE
import Data.Time.LocalTime
import Numeric (showHex)
import System.Directory
import System.IO
import Prelude as P

-- TODO remove, we want to read all the files
basePath :: FilePath
basePath = "./raw/2020-06-24/"

-- | This is messy. We take the part of the filename containing the timestamp,
-- encode it to a JSON string and then decode it, as Aeson can parse a
-- LocalTime from String and I couldn't find any other method to do that...
parseTime :: String -> LocalTime
parseTime str = fromJust $ Aeson.decode $ Aeson.encode str

-- | The deserialization of the per vehicle object from the HAFAS JSON.
-- Unfortunately we can't really store the timestamp in the structure, as it is
-- not part of the JSON, so we have to carry it separately..
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

-- | Filter function to grap specific datapoints, e.g. everything from one ride
-- or one tram line.
type Filter = (LocalTime, Vehicle) -> Bool

-- | Read all timestamps and Vehicles from a .json.gz file, which is itself the
-- result of one HAFAS API request. We do not filter here.
getVehicles :: FilePath -> IO [(LocalTime, Vehicle)]
getVehicles path =
  let timeStamp = parseTime $ P.take 19 path
   in do
        gzippedContent <- BL.readFile $ basePath ++ path
        case (Aeson.eitherDecode $ GZ.decompress gzippedContent) of
          (Right res) -> do
            return $ P.zip (P.repeat timeStamp) res
          (Left err) -> do
            System.IO.hPutStrLn stderr err
            return []

-- | Read a list of .json.gz files in, decode them and filter them using Filter
-- functions.
getAllVehicles :: [FilePath] -> Filter -> IO [(LocalTime, Vehicle)]
getAllVehicles [] _ = return []
getAllVehicles (f : fs) vehicleFilter = do
  vehicles <- getVehicles f
  nextVehicles <- getAllVehicles fs vehicleFilter
  return $ (P.filter vehicleFilter vehicles) ++ nextVehicles

-- Fahrt von Marie-Juchacz-Str nach Campus Jungfernsee, 2020-06-24 12:11 bis 12:52
filter96 :: Filter
filter96 (t, v) =
  trip v == 53928
    && t >= parseTime "2020-06-24 12:11:28" -- Marie-Juchacz-Str
    && t <= parseTime "2020-06-24 12:48:43" -- Rote Kaserne

main :: IO ()
main = do
  fileList <- listDirectory basePath
  vehicles <- getAllVehicles fileList filter96
  P.putStrLn
    $ join "\n"
    $ P.map
      (\(t, v) -> (show t) ++ ", " ++ (show $ latitude v) ++ ", " ++ (show $ longitude v))
    $ sortBy (\a -> \b -> compare (fst a) (fst b)) vehicles
