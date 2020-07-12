{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Codec.Compression.GZip as GZ
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson as Aeson
import Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 as BLU
import Data.ByteString.UTF8 as BSU
import Data.Functor
import Data.Geospatial
import Data.IntMap.Strict as IntMap
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Text as TS
import Data.Text.Encoding as TSE
import Data.Text.IO as TSIO
import Data.Text.Lazy.Encoding as TLE
import Data.Time.LocalTime
import GHC.Generics
import Graphics.Svg
import Numeric (showHex)
import Streaming
import Streaming.Osm
import Streaming.Osm.Types
import Streaming.Prelude as S
import System.Directory
import System.IO
import Prelude as P

-- TODO remove, we want to read all the files
basePath :: FilePath
basePath = "./raw/2020-06-24/"

-- | Earth radius in meters
earthRadius :: Double
earthRadius = 6371000

-- | 3D position on earths surface in meters.
type Vec = (Double, Double, Double)

-- | Geo coordinate
type GeoCoord = (Latitude, Longitude)

-- | project a point on earths surface from geocoordinates to a 3D position in meters.
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

-- | Distance in meters between two GeoCoord.
distance :: GeoCoord -> GeoCoord -> Meter
distance a b = vecLength $ vecSubtract (geoToVec a) (geoToVec b)

-- | A and B are two connected Locations on the exact track. C is our inexact
-- mesurement. This function maps C to the track andreturns the mapped distance
-- we treveled from A in meters.
-- Example:
-- mapToTrack (52.415193, 13.050288) (52.415795, 13.050324) (52.415283, 13.050306)
-- This should Just be about 10.3 meters.
mapToTrack :: GeoCoord -> GeoCoord -> GeoCoord -> Maybe Double
mapToTrack a b c =
  let (abx, aby, abz) = vecSubtract (geoToVec a) (geoToVec b)
      (acx, acy, acz) = vecSubtract (geoToVec a) (geoToVec c)
      abLength = vecLength (abx, aby, abz)
      res = (abx * acx + aby * acy + abz * acz) / abLength
   in if 0 <= res && res <= abLength
        then Just res
        else Nothing

-- | This is messy. We take the part of the filename containing the timestamp,
-- encode it to a JSON string and then decode it, as Aeson can parse a
-- LocalTime from String and I couldn't find any other method to do that...
parseTime' :: String -> LocalTime
parseTime' str = fromJust $ Aeson.decode $ Aeson.encode str

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

-- | All the data about one Tram connection, e.g. Line 96 from
-- Marie-Juchacz-Straße to Campus Jungfernsee. The outer list contains the
-- individual segments, that are drawn in the final graphic.
data Line = Line Text [(TripId, [(LocalTime, GeoCoord)])]

type TripId = Int

-- | Filter function to grab specific datapoints, e.g. everything from one ride
-- or one tram line.
data Filter = Filter Text ((LocalTime, Vehicle) -> Bool)

-- | So we can combine Filters with (<>).
instance Semigroup Filter where
  (Filter name1 f) <> (Filter name2 g) = Filter
    (name1 <> "-" <> name2)
    $ \x -> (f x) || (g x)

-- | Read all timestamps and Vehicles from a .json.gz file, which is itself the
-- result of one HAFAS API request. We do not filter here.
getVehicles :: FilePath -> IO [(LocalTime, Vehicle)]
getVehicles path =
  let timeStamp = parseTime' $ P.take 19 path
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
getAllVehicles (f : fs) (Filter filterName vehicleFilter) = do
  vehicles <- getVehicles f
  nextVehicles <- getAllVehicles fs $ Filter filterName vehicleFilter
  return $ (P.filter vehicleFilter vehicles) ++ nextVehicles

-- | This is a proxy for getAllVehicles, but uses a cached JSON file in
-- ./cache/ that is named after the used Filter.
getAllVehiclesCached :: [FilePath] -> Filter -> IO Line
getAllVehiclesCached fileList (Filter filterName vehicleFilter) =
  let cacheName = "./cache/" <> filterName <> ".json"
      cachePath = TS.unpack cacheName
      -- Stupid conversion functions I needed to write in order to make JSON
      -- serializaion possible.
      fromCache :: [(TripId, [(LocalTime, GeoCoord)])] -> Line
      fromCache ds = Main.Line "96" ds
      toCache :: Line -> [(TripId, [(LocalTime, GeoCoord)])]
      toCache (Main.Line _ ds) = ds
   in do
        fileExists <- doesFileExist cachePath
        if fileExists
          then do
            TSIO.putStrLn $ "Cache hit: " <> cacheName
            cacheContent <- BL.readFile cachePath
            return $ fromCache $ fromJust $ Aeson.decode cacheContent
          else do
            TSIO.putStrLn $ "Cache miss: " <> cacheName
            rawRes <- getAllVehicles fileList (Filter filterName vehicleFilter)
            let res = transformVehicles rawRes
             in do
                  BL.writeFile cachePath $ Aeson.encode $ toCache res
                  return res

compareTimeStamp :: (LocalTime, GeoCoord) -> (LocalTime, GeoCoord) -> Ordering
compareTimeStamp a b = compare (fst a) (fst b)

-- | Split when the points are more than 277m apart, the distance a Tram at
-- 100km/h travels in 10s.
splitPredicate :: (LocalTime, GeoCoord) -> (LocalTime, GeoCoord) -> Bool
splitPredicate (t1, p1) (t2, p2) = distance p1 p2 > 277

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

-- | A lot of logic happens here, as the raw data is in no good shape for our use case. We work on all the data points belonging to a tram line and partition them into ordered segments, which correspond to uninterrupted paths in the final graphic. This partitioning is non-triveal, as the following conditions must be true:
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
   in Main.Line "96" $ splitTrips mapByTripId

-- | All data points for Tram 96, in both directions.
filter96 :: Filter
filter96 = Filter "filter96" $ \(_, v) -> tramId v == "96"

type ReferenceTrack = [(Meter, GeoCoord)]

data ReferenceTrackJson
  = ReferenceTrackJson
      { label :: Text,
        coordinates :: [GeoCoord],
        stations :: [(Text, GeoCoord)]
      }
  deriving (Generic, Show)

instance FromJSON ReferenceTrackJson

readReferenceTrackFromFile :: IO ReferenceTrack
readReferenceTrackFromFile = do
  fileContent <- BL.readFile "./cache/96.json"
  return $ enrichTrackWithLength 0 $ coordinates $ fromJust $ Aeson.decode fileContent

type Meter = Double

-- | This returns the distance a tram has traveled, starting from the start of the track, as a ratio of the overall track length. Shouldn't be less than 0 and greater than 1.0, but I'm not sure!
locateCoordOnTrackLength :: ReferenceTrack -> GeoCoord -> Maybe Double
locateCoordOnTrackLength track coord =
  let compareByDistance (_, a) (_, b) = compare (distance coord a) (distance coord b)
      compareByPosition (a, _) (b, _) = compare a b
      overallTrackLength = fst $ P.last track
      -- The two trackpoints closest to coord, sorted by their position on the track.
      twoClosestTrackPoints =
        sortBy compareByPosition
          $ P.take 2
          $ sortBy compareByDistance track
      (currentMark, firstPoint) = twoClosestTrackPoints !! 0
      (_, secondPoint) = twoClosestTrackPoints !! 1
   in case mapToTrack firstPoint secondPoint coord of
        (Just v) -> Just $ (currentMark + v) / overallTrackLength
        Nothing -> Nothing

-- | Put the current kilometre mark on the track. Needs a starting Meter, as it
-- operates recursviely.
enrichTrackWithLength :: Meter -> [GeoCoord] -> ReferenceTrack
enrichTrackWithLength m (x : []) = (m, x) : []
enrichTrackWithLength m (x : next : xs) =
  let cursor = 0
   in (m, x) : (enrichTrackWithLength (m + distance x next) (next : xs))

-- document root
svg :: Element -> Element
svg content =
  doctype
    <> Graphics.Svg.with
      (svg11_ content)
      [Version_ <<- "1.1", Width_ <<- "8640", Height_ <<- "200", ViewBox_ <<- "0 0 8640 200"]

-- | Seconds from midnight on a TimeOfDay
seconds :: TimeOfDay -> Double
seconds (TimeOfDay h m s) =
  fromIntegral $
    3600 * h
      + 60 * m
      -- Yeah… Seriously. That's how I get the seconds out of a TimeOfDay.
      + div (fromEnum s) 1000000000000

-- | Transforms a [(LocalTime -> Double)] and two placement functions fx, fy to
-- an SVG Element. Recursively calls tripToElement'.
tripToElement ::
  (LocalTime -> Double) ->
  (GeoCoord -> Maybe Double) ->
  (TripId, [(LocalTime, GeoCoord)]) ->
  Element
tripToElement _ _ (_, []) = mempty
-- For Trips with a single point we draw a circle instead of a path, as otherwise the path wouldn't be visible.
tripToElement fx fy (_, (t, v) : []) = case (fy v) of
  Just y ->
    circle_
      [ Cx_ <<- (toText $ fx t),
        Cy_ <<- (toText y),
        R_ <<- "0.5",
        Stroke_ <<- "black",
        Fill_ <<- "none"
      ]
  Nothing -> mempty
tripToElement fx fy (tripId, (t, v) : tripData) = case (fy v) of
  Just y ->
    path_
      [ D_ <<- (mA (fx t) y <> (tripToElement' fx fy tripData)),
        Stroke_ <<- "black",
        Fill_ <<- "none",
        Stroke_width_ <<- "1",
        Stroke_linecap_ <<- "round"
        -- Id_ <<- ((<>) "trip" $ TS.pack $ P.show tripId)
      ]
  Nothing -> tripToElement fx fy (tripId, tripData)

-- | Recursively generate the tail of the path elements.
tripToElement' ::
  (LocalTime -> Double) ->
  (GeoCoord -> Maybe Double) ->
  [(LocalTime, GeoCoord)] ->
  Text
tripToElement' _ _ [] = ""
tripToElement' fx fy ((t, v) : ds) = case (fy v) of
  Just y -> lA (fx t) y <> tripToElement' fx fy ds
  Nothing -> tripToElement' fx fy ds

-- | Transforms a Line to an SVG ELement.
lineToElement :: ReferenceTrack -> Line -> Element
lineToElement referenceTrack (Main.Line label trips) =
  let fx t = (*) 0.1 $ seconds $ localTimeOfDay t
      fy v = fmap (200 *) $ locateCoordOnTrackLength referenceTrack v
   in g_ []
        $ P.mconcat
        $ P.map (tripToElement fx fy)
        $ trips

-- | This code is never used, but maybe when
-- https://github.com/fosskers/streaming-osm/issues/3 is resolved?
extractReferenceTrackCached :: IO [ReferenceTrack]
extractReferenceTrackCached =
  let dataPath = "raw/brandenburg-latest.osm.pbf"
      cachePath = "cache/reference-tracks.json"
      filterRelations :: Relation -> Bool
      filterRelations (Relation {_rinfo = Nothing}) = False
      filterRelations (Relation {_rinfo = Just (Info {_id = relationId})}) = relationId == 178663 -- Tram96
      osmToRefTrack :: Relation -> ReferenceTrack
      osmToRefTrack _ = [] -- TODO
   in do
        fileExists <- doesFileExist cachePath
        if fileExists
          then do
            TSIO.putStrLn $ "Cache hit: " <> TS.pack cachePath
            cacheContent <- BL.readFile cachePath
            return $ fromJust $ Aeson.decode cacheContent
          else do
            TSIO.putStrLn $ "Cache miss: " <> TS.pack cachePath
            osmRelations <- runResourceT . toList_ . S.filter filterRelations $ relations . blocks $ blobs dataPath
            TSIO.putStrLn $ TS.pack $ P.show osmRelations
            let referenceTracks = P.map osmToRefTrack osmRelations
             in do
                  -- BL.writeFile cachePath $ Aeson.encode referenceTracks
                  return referenceTracks

-- | Show a line as an CSV table, just a helper function I'll not use often.
printCSV :: Line -> String
printCSV (Main.Line _ trips) =
  Data.List.Utils.join "\n"
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

main :: IO ()
main = do
  fileList <- listDirectory basePath
  trips <- getAllVehiclesCached fileList $ filter96
  referenceTrack <- readReferenceTrackFromFile
  P.print "fertig"
  P.writeFile "96.svg" $ P.show $ svg $ lineToElement referenceTrack trips
