-- SPDX-FileCopyrightText: 2020 Kerstin Humm <mail@erictapen.name>
--
-- SPDX-License-Identifier: GPL-3.0-or-later
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hafas
  ( Line (Line),
    TripId,
    getAllVehiclesCached,
    filterTram,
    Day (Saturday, Monday, SomeDay),
    seconds,
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

-- | A distinction so we can use only the first 3 hours of a saturday and all
-- but the first 3 hours of a monday. This way we make sure to really use only
-- weekday data.
data Day = Saturday String | Monday String | SomeDay String

-- | Seconds from midnight on a TimeOfDay
seconds :: TimeOfDay -> Double
seconds (TimeOfDay h m s) =
  fromIntegral $
    3600 * h
      + 60 * m
      -- Yeah… Seriously. That's how I get the seconds out of a TimeOfDay.
      + div (fromEnum s) 1000000000000

-- | The path were the raw data for a given day is located.
dayPath :: Day -> FilePath
dayPath (Saturday s) = "./raw/" <> s <> "/"
dayPath (Monday s) = "./raw/" <> s <> "/"
dayPath (SomeDay s) = "./raw/" <> s <> "/"

-- | True if a given day contains the timestamp. From saturdays we only want
-- the first 3 hours. From mondays only the last 21 hours.
dayContains :: Day -> LocalTime -> Bool
dayContains (Saturday _) time = 3600 * 3 < (seconds $ localTimeOfDay time)
dayContains (Monday _) time = 3600 * 3 >= (seconds $ localTimeOfDay time)
dayContains (SomeDay _) _ = True

allDays :: [Day]
allDays =
  [ -- We don't use 2020-06-17, as it is incomplete
    SomeDay "2020-06-18",
    SomeDay "2020-06-19",
    Saturday "2020-06-20",
    Monday "2020-06-22",
    SomeDay "2020-06-23",
    SomeDay "2020-06-24",
    SomeDay "2020-06-25",
    SomeDay "2020-06-26",
    Saturday "2020-06-27",
    Monday "2020-06-29",
    SomeDay "2020-06-30",
    SomeDay "2020-07-01",
    SomeDay "2020-07-02",
    SomeDay "2020-07-03",
    Saturday "2020-07-04",
    Monday "2020-07-06",
    SomeDay "2020-07-07",
    SomeDay "2020-07-08",
    SomeDay "2020-07-09",
    SomeDay "2020-07-10",
    Saturday "2020-07-11",
    Monday "2020-07-13",
    SomeDay "2020-07-14",
    SomeDay "2020-07-15",
    SomeDay "2020-07-16"
  ]

-- | Show a line as an CSV table, just a helper function I'll not use often.
printCSV :: Line -> String
printCSV (Line _ trips) =
  (++) "id, time, lat, lon\n" $
    Data.List.Utils.join "\n" $
      P.concat $
        P.map
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
        IntMap.toList $
          IntMap.map (sortBy compareTimeStamp) $
            fromListWith (++) $
              P.map transformDatapoint vehicles
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
data Vehicle = Vehicle
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
getVehicles :: Day -> Filter -> FilePath -> IO [(LocalTime, Vehicle)]
getVehicles day (Filter _ vehicleFilter) path =
  -- ./raw/2020-07-08/2020-07-08T16:05:14+02:00.json.gz
  -- We want this:    ^^^^^^^^^^^^^^^^^^^
  let timeStamp = parseTime' $ P.take 19 $ P.drop 17 path
   in if dayContains day timeStamp
        then do
          gzippedContent <- BL.readFile path
          if BL.length gzippedContent == 20
            then do
              -- When the crawler failed to fetch something, it produced an empty gzip header.
              return []
            else do
              case (Aeson.eitherDecode $ GZ.decompress gzippedContent) of
                (Right res) -> do
                  return $ P.zip (P.repeat timeStamp) $ P.filter vehicleFilter res
                (Left err) -> do
                  System.IO.hPutStrLn stderr err
                  return []
        else do return []

-- | Read a list of .json.gz files in, decode them and filter them using Filter
-- functions.
getAllVehicles :: Day -> Filter -> IO [(LocalTime, Vehicle)]
getAllVehicles day filter = do
  files <- listDirectory $ dayPath day
  TSIO.putStrLn $ "Reading " <> (TS.pack $ P.show $ P.length files) <> " gzip files from " <> (TS.pack $ dayPath day) <> "."
  vehicles <- parallel $ P.map (\f -> getVehicles day filter $ dayPath day <> f) files
  return $ mconcat vehicles

-- | This is a proxy for getAllVehicles, but uses a cached JSON file in
-- ./cache/ that is named after the used Filter.
getAllVehiclesCached :: (Maybe Day) -> Filter -> IO Line
getAllVehiclesCached day filter@(Filter filterName vehicleFilter) =
  let cacheName = case day of
        Just (SomeDay d) -> "./cache/" <> (TS.pack d) <> "-" <> filterName <> ".json"
        Just _ -> error "not implemented."
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
            TSIO.putStrLn $ "Cache hit:  " <> cacheName
            cacheContent <- BL.readFile cachePath
            case (Aeson.eitherDecode cacheContent) of
              (Right res) -> do
                return $ fromCache res
              (Left err) -> do
                System.IO.hPutStrLn stderr $ "Can't read from cache " <> cachePath <> ": " <> err
                return $ Line "" []
          else do
            TSIO.putStrLn $ "Cache miss: " <> cacheName
            rawRes <- sequence $
              P.map
                ( \day -> do
                    getAllVehicles day filter
                )
                $ case day of
                  Just d -> [d]
                  Nothing -> allDays
            let res = transformVehicles $ mconcat rawRes
             in do
                  BL.writeFile cachePath $ Aeson.encode $ toCache res
                  return res
