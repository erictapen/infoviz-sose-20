{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hafas
  ( Line (Line),
    TripId,
    getAllVehiclesCached,
    filterTram,
  )
where

import Codec.Compression.GZip as GZ
import Control.Concurrent.ParallelIO.Global
import Data.Aeson as Aeson
import Data.ByteString.Lazy as BL
import qualified Data.Geospatial as Geospatial
import Data.IntMap.Strict as IntMap
import Data.List
import Data.List.Utils
import Data.Text as TS
import Data.Text.IO as TSIO
import Data.Time.LocalTime
import Geo
import Graphics.Svg
import System.Directory
import System.IO
import Prelude as P

-- | All the data about one Tram connection, e.g. Line 96 from
-- Marie-Juchacz-Straße to Campus Jungfernsee. The outer list contains the
-- individual segments, that are drawn in the final graphic.
data Line = Line Text [(TripId, [(LocalTime, GeoCoord)])]

type TripId = Int

allDays :: [String]
allDays =
  [ -- We don't use 2020-06-17, as it is incomplete
    "2020-06-18",
    "2020-06-19",
    "2020-06-22",
    "2020-06-23",
    "2020-06-24",
    "2020-06-25",
    "2020-06-26",
    "2020-06-29",
    "2020-06-30",
    "2020-07-01",
    "2020-07-02",
    "2020-07-03",
    "2020-07-06",
    "2020-07-07",
    "2020-07-08",
    "2020-07-09",
    "2020-07-10",
    "2020-07-13",
    "2020-07-14",
    "2020-07-15",
    "2020-07-16"
  ]

-- | Show a line as an CSV table, just a helper function I'll not use often.
printCSV :: Line -> String
printCSV (Line _ trips) =
  (++) "id, time, lat, lon\n"
    $ Data.List.Utils.join "\n"
    $ P.concat
    $ P.map
      ( \(tripId, ds) ->
          P.map
            ( \(t, c) ->
                (P.show tripId)
                  ++ ","
                  ++ (P.show t)
                  ++ ","
                  ++ (P.show $ fst c)
                  ++ ","
                  ++ (P.show $ snd c)
            )
            ds
      )
      trips

-- | This is messy. We take the part of the filename containing the timestamp,
-- encode it to a JSON string and then decode it, as Aeson can parse a
-- LocalTime from String and I couldn't find any other method to do that...
parseTime' :: String -> LocalTime
parseTime' str = case (Aeson.eitherDecode $ Aeson.encode str) of
  (Right res) -> res
  (Left res) -> error $ "Couldn't deserialise date " <> str

compareTimeStamp :: (LocalTime, GeoCoord) -> (LocalTime, GeoCoord) -> Ordering
compareTimeStamp a b = compare (fst a) (fst b)

-- | Split when the points are more than 277m apart, the distance a Tram at
-- 100km/h travels in 10s.
splitPredicate :: (LocalTime, GeoCoord) -> (LocalTime, GeoCoord) -> Bool
splitPredicate (_, p1) (_, p2) = distance (geoToPoint p1) (geoToPoint p2) > 277 -- (100000 / 360)

-- | Example:
-- splitTrip [(parseTime' "2020-07-09 12:00:05", (13.068, 52.383)), (parseTime' "2020-07-09 12:00:10", (13.053, 52.402))]
splitTrip :: [(LocalTime, GeoCoord)] -> [[(LocalTime, GeoCoord)]]
splitTrip [] = []
splitTrip ds =
  let seg = splitTrip' ds
   in (seg : (splitTrip $ P.drop (P.length seg) ds))

splitTrip' :: [(LocalTime, GeoCoord)] -> [(LocalTime, GeoCoord)]
splitTrip' [] = []
splitTrip' (a : []) = a : []
splitTrip' (a : b : ds) =
  if splitPredicate a b
    then (a : [])
    else (a : (splitTrip' (b : ds)))

-- | A lot of logic happens here, as the raw data is in no good shape for our use case. We work on all the data points belonging to a tram line and partition them into ordered segments, which correspond to uninterrupted paths in the final graphic. This partitioning is non-trivial, as the following conditions must be true:
-- - Every path contains only data points of one tripId.
-- - There can be multiple paths with the same tripId.
-- - The data points in each path are sorted by time.
-- - No path has a gap, where the points are more than a certain time/geodistance apart.
transformVehicles :: [(LocalTime, Vehicle)] -> Line
transformVehicles vehicles =
  let -- Bring the data into a structure where we can access TripId more
      -- easily.
      transformDatapoint :: (LocalTime, Vehicle) -> (TripId, [(LocalTime, GeoCoord)])
      transformDatapoint (t, v) = (trip v, [(t, (latitude v, longitude v))])
      -- Every "bucket" belongs to exactly one TripId. The datapoints in the
      -- buckets are sorted.
      mapByTripId :: [(TripId, [(LocalTime, GeoCoord)])]
      mapByTripId =
        IntMap.toList
          $ IntMap.map (sortBy compareTimeStamp)
          $ fromListWith (++)
          $ P.map transformDatapoint vehicles
      -- Split the "trips" along a splitPredicate. Whenever splitPredicate is
      -- true for two data points, they should not be connected in the final
      -- result.
      splitTrips :: [(TripId, [(LocalTime, GeoCoord)])] -> [(TripId, [(LocalTime, GeoCoord)])]
      splitTrips [] = []
      splitTrips ((tripId, ds) : trips) =
        let splitted :: [(TripId, [(LocalTime, GeoCoord)])]
            splitted = P.map (\ds' -> (tripId, ds')) $ splitTrip ds
         in splitted ++ splitTrips trips
   in Line "" $ splitTrips mapByTripId

-- | Filter function to grab specific datapoints, e.g. everything from one ride
-- or one tram line.
data Filter = Filter Text (Vehicle -> Bool)

-- | So we can combine Filters with (<>).
instance Semigroup Filter where
  (Filter name1 f) <> (Filter name2 g) = Filter
    (name1 <> "-" <> name2)
    $ \x -> (f x) || (g x)

-- | All data points for e.g. Tram 96, in both directions.
filterTram :: Text -> Filter
filterTram tram = Filter ("filter" <> tram) $ \v -> tramId v == tram

-- | The deserialization of the per vehicle object from the HAFAS JSON.
-- Unfortunately we can't really store the timestamp in the structure, as it is
-- not part of the JSON, so we have to carry it separately..
data Vehicle
  = Vehicle
      { tramId :: Text,
        latitude :: Geospatial.Latitude,
        longitude :: Geospatial.Longitude,
        trip :: Int
      }
  deriving (Show)

-- | Vehicle is directly derived from the JSON data.
instance FromJSON Vehicle where
  parseJSON = withObject "Vehicle" $ \o -> do
    line <- o .: "line"
    trip <- o .: "trip"
    tramId <- line .: "id"
    location <- o .: "location"
    latitude <- location .: "latitude"
    longitude <- location .: "longitude"
    return Vehicle {..}

instance ToJSON Vehicle where
  toJSON (Vehicle {tramId = tramId, latitude = latitude, longitude = longitude, trip = trip}) =
    object
      [ "line" .= (object ["id" .= tramId]),
        "trip" .= trip,
        "location" .= (object ["latitude" .= latitude, "longitude" .= longitude])
      ]

-- | Read all timestamps and Vehicles from a .json.gz file, which is itself the
-- result of one HAFAS API request. We do not filter here.
getVehicles :: Filter -> FilePath -> IO [(LocalTime, Vehicle)]
getVehicles (Filter _ vehicleFilter) path =
  -- ./raw/2020-07-08/2020-07-08T16:05:14+02:00.json.gz
  -- We want this:    ^^^^^^^^^^^^^^^^^^^
  let timeStamp = parseTime' $ P.take 19 $ P.drop 17 path
   in do
        gzippedContent <- BL.readFile path
        case (Aeson.eitherDecode $ GZ.decompress gzippedContent) of
          (Right res) -> do
            return $ P.zip (P.repeat timeStamp) $ P.filter vehicleFilter res
          (Left err) -> do
            System.IO.hPutStrLn stderr err
            return []

-- | Read a list of .json.gz files in, decode them and filter them using Filter
-- functions.
getAllVehicles :: FilePath -> [FilePath] -> Filter -> IO [(LocalTime, Vehicle)]
getAllVehicles basePath files filter = do
  vehicles <- parallel $ P.map (\f -> getVehicles filter $ basePath <> f) files
  return $ mconcat vehicles

-- | This is a proxy for getAllVehicles, but uses a cached JSON file in
-- ./cache/ that is named after the used Filter.
getAllVehiclesCached :: (Maybe String) -> Filter -> IO Line
getAllVehiclesCached day filter@(Filter filterName vehicleFilter) =
  let cacheName = case day of
        Just d -> "./cache/" <> (TS.pack d) <> "-" <> filterName <> ".json"
        Nothing -> "./cache/all_days-" <> filterName <> ".json"
      cachePath = TS.unpack cacheName
      -- Stupid conversion functions I needed to write in order to make JSON
      -- serialization possible.
      fromCache :: [(TripId, [(LocalTime, GeoCoord)])] -> Line
      fromCache ds = Line filterName ds
      toCache :: Line -> [(TripId, [(LocalTime, GeoCoord)])]
      toCache (Line _ ds) = ds
   in do
        fileExists <- doesFileExist cachePath
        if fileExists
          then do
            TSIO.putStrLn $ "Cache hit: " <> cacheName
            cacheContent <- BL.readFile cachePath
            case (Aeson.eitherDecode cacheContent) of
              (Right res) -> do
                return $ fromCache res
              (Left err) -> do
                System.IO.hPutStrLn stderr $ "Can't read from cache " <> cachePath <> ": " <> err
                return $ Line "" []
          else do
            TSIO.putStrLn $ "Cache miss: " <> cacheName
            rawRes <- parallel
              $ P.map
                ( \d -> do
                    fileList <- listDirectory $ "./raw/" <> d <> "/"
                    getAllVehicles ("./raw/" <> d <> "/") fileList filter
                )
              $ case day of
                Just d -> [d]
                Nothing -> allDays
            let res = transformVehicles $ mconcat rawRes
             in do
                  BL.writeFile cachePath $ Aeson.encode $ toCache res
                  return res
