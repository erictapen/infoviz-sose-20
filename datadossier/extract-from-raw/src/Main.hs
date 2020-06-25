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

-- | Earth radius in meters
earthRadius :: Double
earthRadius = 6371000

type Vec = (Double, Double, Double)

type GeoCoord = (Latitude, Longitude)

geoToVec :: GeoCoord -> Vec
geoToVec (lat, lon) =
  let latR = lat * pi / 180
      lonR = lon * pi / 180
   in ( earthRadius * cos lonR * cos latR,
        earthRadius * sin lonR * cos latR,
        earthRadius * sin latR
      )

-- | Get the length of a 3D vector
-- Example:
-- vecLength $ vecSubtract (geoToVec 52.359792 13.137204) (geoToVec 52.424919 13.053749)
vecLength :: Vec -> Double
vecLength (x, y, z) = sqrt $ x * x + y * y + z * z

-- | vecSubtract a b = vector from a to b (or ab)
vecSubtract :: Vec -> Vec -> Vec
vecSubtract (x1, y1, z1) (x2, y2, z2) = (x2 - x1, y2 - y1, z2 - z1)

-- | A and B are two connected Locations on the exact track. C is our inexact
-- mesurement. This function maps C to the track andreturns the mapped distance
-- we treveled from A in meters.
-- Example:
-- mapToTrack (52.415193, 13.050288) (52.415795, 13.050324) (52.415283, 13.050306)
-- This should be about 10.3 meters.
mapToTrack :: GeoCoord -> GeoCoord -> GeoCoord -> Double
mapToTrack a b c =
  let (abx, aby, abz) = vecSubtract (geoToVec a) (geoToVec b)
      (acx, acy, acz) = vecSubtract (geoToVec a) (geoToVec c)
   in (abx * acx + aby * acy + abz * acz) / vecLength (abx, aby, abz)

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
      { tramId :: Text,
        latitude :: Latitude,
        longitude :: Longitude,
        trip :: Int
      }
  deriving (Show)

instance FromJSON Vehicle where
  parseJSON = withObject "Vehicle" $ \o -> do
    line <- o .: "line"
    trip <- o .: "trip"
    tramId <- line .: "id"
    location <- o .: "location"
    latitude <- location .: "latitude"
    longitude <- location .: "longitude"
    return Vehicle {..}

-- | Filter function to grap specific datapoints, e.g. everything from one ride
-- or one tram line.
data Filter = Filter ((LocalTime, Vehicle) -> Bool)

instance Semigroup Filter where
  (Filter f) <> (Filter g) = Filter $ \x -> (f x) || (g x)

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
getAllVehicles (f : fs) (Filter vehicleFilter) = do
  vehicles <- getVehicles f
  nextVehicles <- getAllVehicles fs $ Filter vehicleFilter
  return $ (P.filter vehicleFilter vehicles) ++ nextVehicles

-- | Fahrt von Marie-Juchacz-Str nach Campus Jungfernsee, 2020-06-24 12:11 bis 12:52
filter96Track :: Filter
filter96Track = Filter $ \(t, v) ->
  trip v == 53928
    && t >= parseTime "2020-06-24 12:11:28" -- Marie-Juchacz-Str
    && t <= parseTime "2020-06-24 12:48:43" -- Rote Kaserne

-- | All data points for Tram 96, in both directions.
filter96 :: Filter
filter96 = Filter $ \(_, v) -> tramId v == "96"

main :: IO ()
main = do
  fileList <- listDirectory basePath
  vehicles <- getAllVehicles fileList $ filter96Track <> filter96
  P.putStrLn
    $ join "\n"
    $ P.map
      (\(t, v) -> (show t) ++ ", " ++ (show $ latitude v) ++ ", " ++ (show $ longitude v))
    $ sortBy (\a -> \b -> compare (fst a) (fst b)) vehicles
