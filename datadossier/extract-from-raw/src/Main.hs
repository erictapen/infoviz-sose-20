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
import Data.IntMap.Strict as IntMap
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Text as TS
import Data.Text.Encoding as TSE
import Data.Text.IO as TSIO
import Data.Text.Lazy.Encoding as TLE
import Data.Time.LocalTime
import Graphics.Svg
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
-- Marie-Juchacz-Straße to Campus Jungfernsee. The key in the IntMap is the
-- trip id from raw data.
type Line = IntMap [(LocalTime, Vehicle)]

-- | Filter function to grap specific datapoints, e.g. everything from one ride
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
   in do
        fileExists <- doesFileExist cachePath
        if fileExists
          then do
            TSIO.putStrLn $ "Cache hit: " <> cacheName
            cacheContent <- BL.readFile cachePath
            return $ fromJust $ Aeson.decode cacheContent
          else do
            TSIO.putStrLn $ "Cache miss: " <> cacheName
            rawRes <- getAllVehicles fileList (Filter filterName vehicleFilter)
            let res = transformVehicles rawRes
             in do
                  BL.writeFile cachePath $ Aeson.encode res
                  return res

compareTimeStamp :: (LocalTime, Vehicle) -> (LocalTime, Vehicle) -> Ordering
compareTimeStamp a b = compare (fst a) (fst b)

transformVehicles :: [(LocalTime, Vehicle)] -> Line
transformVehicles vehicles =
  let mapWithUnorderedLists = fromListWith (++) $ P.map (\(t, v) -> (trip v, [(t, v)])) vehicles
   in IntMap.map (sortBy compareTimeStamp) mapWithUnorderedLists

-- | Fahrt von Marie-Juchacz-Str nach Campus Jungfernsee, 2020-06-24 12:11 bis 12:52
filter96Track :: Filter
filter96Track = Filter "filter96Track" $ \(t, v) ->
  trip v == 53928
    && t >= parseTime' "2020-06-24 12:11:28" -- Marie-Juchacz-Str
    && t <= parseTime' "2020-06-24 12:48:43" -- Rote Kaserne

-- | All data points for Tram 96, in both directions.
filter96 :: Filter
filter96 = Filter "filter96" $ \(_, v) -> tramId v == "96"

type Track = [(Meter, GeoCoord)]

type Meter = Double

-- | This returns the distance a tram has traveled, starting from the start of the track.
locateCoordOnTrackLength :: Track -> GeoCoord -> Maybe Meter
locateCoordOnTrackLength track coord =
  let compareByDistance (_, a) (_, b) = compare (distance coord a) (distance coord b)
      compareByPosition (a, _) (b, _) = compare a b
      -- The two trackpoints closest to coord, sorted by their position on the track.
      twoClosestTrackPoints =
        sortBy compareByPosition
          $ P.take 2
          $ sortBy compareByDistance track
      (currentMark, firstPoint) = twoClosestTrackPoints !! 0
      (_, secondPoint) = twoClosestTrackPoints !! 1
   in case mapToTrack firstPoint secondPoint coord of
        (Just v) -> Just $ currentMark + v
        Nothing -> Nothing

-- | Put the current kilometre mark on the track. Needs a starting Meter, as it
-- operates recursviely.
enrichTrackWithLength :: Meter -> [GeoCoord] -> Track
enrichTrackWithLength m (x : []) = (m, x) : []
enrichTrackWithLength m (x : next : xs) =
  let cursor = 0
   in (m, x) : (enrichTrackWithLength (m + distance x next) (next : xs))

-- document root
svg :: Element -> Element
svg content =
  doctype
    <> with
      (svg11_ content)
      [Version_ <<- "1.1", Width_ <<- "3600mm", Height_ <<- "200mm", ViewBox_ <<- "0 0 200 200"]

-- TODO: remove and replace with Graphics.Svg.Path.toText
showR :: Double -> Text
showR r = TS.pack $ (show r)

-- | Seconds from midnight on a TimeOfDay
seconds :: TimeOfDay -> Double
seconds (TimeOfDay h m s) =
  fromIntegral $
    3600 * h
      + 60 * m
      -- Yeah… Seriously. That's how I get the seconds out of a TimeOfDay.
      + div (fromEnum s) 1000000000000

tripToElement ::
  (LocalTime -> Double) ->
  (Vehicle -> Maybe Double) ->
  (Int, [(LocalTime, Vehicle)]) ->
  Element
tripToElement _ _ (_, []) = mempty
tripToElement fx fy (tripId, (t, v) : tripData) = case (fy v) of
  Just y ->
    path_
      [ D_ <<- (mA (fx t) y <> (tripToElement' fx fy tripData)),
        Stroke_ <<- "black",
        Fill_ <<- "none",
        Stroke_width_ <<- "20"
      ]
  Nothing -> tripToElement fx fy (tripId, tripData)

tripToElement' ::
  (LocalTime -> Double) ->
  (Vehicle -> Maybe Double) ->
  [(LocalTime, Vehicle)] ->
  Text
tripToElement' _ _ [] = ""
tripToElement' fx fy ((t, v) : ds) = case (fy v) of
  Just y -> lA (fx t) y <> tripToElement' fx fy ds
  Nothing -> tripToElement' fx fy ds

lineToElement :: Line -> Element
lineToElement line =
  let -- track96 is the list of coordinates on Track 96, sorted from MJ-Str to Campus Jungfernsee.
      track96 :: Track
      track96 =
        enrichTrackWithLength 0
          $ P.map (\(_, v) -> (latitude v, longitude v))
          $ sortBy compareTimeStamp
          -- letssss hope that trip IDs are unique in all trips ever gathered!
          $ fromJust
          $ IntMap.lookup 53928 line
      fx t = (*) 4.0 $ seconds $ localTimeOfDay t
      fy v = locateCoordOnTrackLength track96 (latitude v, longitude v)
   in P.mconcat $ P.map (tripToElement fx fy) $ IntMap.toList line

-- | Generate the SVG Element that shows all the data points.
-- svgFromData :: [(LocalTime, Vehicle)] -> Element
-- svgFromData dataPoints =
--   let
--    in mconcat $ P.map xyToDot $ P.map (\(t, v) -> (t, (latitude v, longitude v))) dataPoints
main :: IO ()
main = do
  fileList <- listDirectory basePath
  vehicles <- getAllVehiclesCached fileList $ filter96Track <> filter96
  print "fertig"
  P.writeFile "96.svg" $ show $ svg $ g_ [] $ lineToElement vehicles
