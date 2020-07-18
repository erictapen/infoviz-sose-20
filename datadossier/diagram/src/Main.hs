{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Codec.Compression.GZip as GZ
import Control.Monad.Trans.Resource
import Data.Aeson as Aeson
import Data.ByteString.Lazy as BL
import Data.Functor
import Data.Geospatial
import Data.IntMap.Strict as IntMap
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Text as TS
import Data.Text.IO as TSIO
import Data.Time.LocalTime
import GHC.Generics
import GHC.IO.Encoding
import Graphics.Svg
import Streaming.Osm
import Streaming.Osm.Types
import Streaming.Prelude as S
import System.Directory
import System.IO
import Prelude as P

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
parseTime' str = case (Aeson.eitherDecode $ Aeson.encode str) of
  (Right res) -> res
  (Left res) -> error $ "Couldn't deserialise date " <> str

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
  -- ./raw/2020-07-08/2020-07-08T16:05:14+02:00.json.gz
  -- We want this:    ^^^^^^^^^^^^^^^^^^^
  let timeStamp = parseTime' $ P.take 19 $ P.drop 17 path
   in do
        gzippedContent <- BL.readFile path
        case (Aeson.eitherDecode $ GZ.decompress gzippedContent) of
          (Right res) -> do
            return $ P.zip (P.repeat timeStamp) res
          (Left err) -> do
            System.IO.hPutStrLn stderr err
            return []

-- | Read a list of .json.gz files in, decode them and filter them using Filter
-- functions.
getAllVehicles :: FilePath -> [FilePath] -> Filter -> IO [(LocalTime, Vehicle)]
getAllVehicles _ [] _ = return []
getAllVehicles basePath (f : fs) (Filter filterName vehicleFilter) = do
  vehicles <- getVehicles $ basePath <> f
  nextVehicles <- getAllVehicles basePath fs $ Filter filterName vehicleFilter
  return $ (P.filter vehicleFilter vehicles) ++ nextVehicles

-- | This is a proxy for getAllVehicles, but uses a cached JSON file in
-- ./cache/ that is named after the used Filter.
getAllVehiclesCached :: [String] -> Filter -> IO [Line]
getAllVehiclesCached [] _ = return []
getAllVehiclesCached (day : days) (Filter filterName vehicleFilter) =
  let cacheName = "./cache/" <> (TS.pack day) <> "-" <> filterName <> ".json"
      cachePath = TS.unpack cacheName
      basePath = "./raw/" <> day <> "/"
      -- Stupid conversion functions I needed to write in order to make JSON
      -- serialization possible.
      fromCache :: [(TripId, [(LocalTime, GeoCoord)])] -> Line
      fromCache ds = Main.Line "96" ds
      toCache :: Line -> [(TripId, [(LocalTime, GeoCoord)])]
      toCache (Main.Line _ ds) = ds
   in do
        fileExists <- doesFileExist cachePath
        nextLine <- getAllVehiclesCached days (Filter filterName vehicleFilter)
        if fileExists
          then do
            TSIO.putStrLn $ "Cache hit: " <> cacheName
            cacheContent <- BL.readFile cachePath
            case (Aeson.eitherDecode cacheContent) of
              (Right res) -> do
                return $ (fromCache $ res) : nextLine
              (Left err) -> do
                System.IO.hPutStrLn stderr $ "Can't read from cache " <> cachePath <> ": " <> err
                return []
          else do
            TSIO.putStrLn $ "Cache miss: " <> cacheName
            fileList <- listDirectory basePath
            rawRes <- getAllVehicles basePath fileList (Filter filterName vehicleFilter)
            let res = transformVehicles rawRes
             in do
                  BL.writeFile cachePath $ Aeson.encode $ toCache res
                  return $ res : nextLine

compareTimeStamp :: (LocalTime, GeoCoord) -> (LocalTime, GeoCoord) -> Ordering
compareTimeStamp a b = compare (fst a) (fst b)

-- | Split when the points are more than 277m apart, the distance a Tram at
-- 100km/h travels in 10s.
splitPredicate :: (LocalTime, GeoCoord) -> (LocalTime, GeoCoord) -> Bool
splitPredicate (_, p1) (_, p2) = distance p1 p2 > 277 -- (100000 / 360)

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

readReferenceTrackFromFile :: IO (ReferenceTrack, [(Text, GeoCoord)])
readReferenceTrackFromFile = do
  fileContent <- BL.readFile "./cache/96.json"
  case (Aeson.eitherDecode fileContent) of
    (Right res) -> return (enrichTrackWithLength 0 $ coordinates res, stations res)
    (Left err) -> error "Can't deserialise Referencetrack."

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
enrichTrackWithLength _ [] = []
enrichTrackWithLength m (x : []) = (m, x) : []
enrichTrackWithLength m (x : next : xs) =
  (m, x) : (enrichTrackWithLength (m + distance x next) (next : xs))

-- document root
svg :: Element -> Element
svg content =
  let width = 8640 + 50 + 20 + 20
      height = 200 + 20 + 20
   in doctype
        <> Graphics.Svg.with
          (svg11_ content)
          [ Version_ <<- "1.1",
            Width_ <<- (toText width),
            Height_ <<- (toText height),
            ViewBox_ <<- "0 0 " <> (toText width) <> " " <> (toText height)
          ]

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
  Text ->
  Double ->
  (LocalTime -> Double) ->
  (GeoCoord -> Maybe Double) ->
  (TripId, [(LocalTime, GeoCoord)]) ->
  Element
tripToElement _ _ _ _ (_, []) = mempty
-- For Trips with a single point we draw a circle instead of a path, as otherwise the path wouldn't be visible.
tripToElement color strokeWidth fx fy (_, (t, v) : []) = case (fy v) of
  Just y ->
    circle_
      [ Cx_ <<- (toText $ fx t),
        Cy_ <<- (toText y),
        R_ <<- (toText $ 0.5 * strokeWidth),
        Stroke_ <<- "none",
        Fill_ <<- color
      ]
  Nothing -> mempty
tripToElement color strokeWidth fx fy (tripId, (t, v) : tripData) = case (fy v) of
  Just y ->
    path_
      [ D_ <<- (mA (fx t) y <> (tripToElement' fx fy tripData)),
        Stroke_ <<- color,
        Fill_ <<- "none",
        Stroke_width_ <<- (toText strokeWidth),
        Stroke_linecap_ <<- "round",
        Id_ <<- ((<>) "trip" $ TS.pack $ P.show tripId)
      ]
  Nothing -> tripToElement color strokeWidth fx fy (tripId, tripData)

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
lineToElement :: Text -> Double -> [(Text, GeoCoord)] -> ReferenceTrack -> [Line] -> Element
lineToElement color strokeWidth stations referenceTrack lines =
  let fx t = (*) 0.1 $ seconds t
      fy v = fmap (200 *) $ locateCoordOnTrackLength referenceTrack v
   in g_
        [ Transform_ <<- translate 20 20
        ]
        $ (yLegend fy stations)
          <> (xLegend fx)
          <> g_
            [ Id_ <<- "diagram",
              Transform_ <<- translate 50 0
            ]
            ( styleElement
                <> ( P.mconcat $
                       P.map
                         ( \(Main.Line lineId trips) ->
                             P.mconcat
                               $ P.map (tripToElement color strokeWidth (fx . localTimeOfDay) fy)
                               $ trips
                         )
                         lines
                   )
            )

formatTime :: TimeOfDay -> Text
formatTime t =
  TS.pack $
    (P.show $ todHour t)
      <> ":"
      <> ( case (todMin t) of
             0 -> "00"
             _ -> (P.show (todMin t))
         )

xLegend :: (TimeOfDay -> Double) -> Element
xLegend fx =
  P.mconcat $
    P.map
      ( \t ->
          ( text_
              [ X_ <<- (toText ((fx t) + 50)),
                Y_ <<- (toText (-5)),
                Font_family_ <<- "Fira Sans",
                Text_anchor_ <<- "middle",
                Style_ <<- "text-align: center;",
                Font_size_ <<- "4"
              ]
              $ toElement
              $ formatTime t
          )
            <> line_
              [ X1_ <<- (toText ((fx t) + 50)),
                X2_ <<- (toText ((fx t) + 50)),
                Y1_ <<- (toText (-3)),
                Y2_ <<- (toText (-1)),
                Stroke_ <<- "black",
                Stroke_width_ <<- "1"
              ]
      )
      [(TimeOfDay h m 0) | h <- [0 .. 23], m <- [0, 10 .. 50]]

yLegend :: (GeoCoord -> Maybe Double) -> [(Text, GeoCoord)] -> Element
yLegend _ [] = mempty
yLegend fy ((label, coord) : stations) =
  ( case (fy coord) of
      Nothing -> mempty
      (Just yPos) ->
        ( ( text_
              [ X_ <<- (toText (50 - 1)),
                Y_ <<- (toText (yPos + 1)),
                Font_family_ <<- "Fira Sans",
                Text_anchor_ <<- "end",
                Style_ <<- "text-align: end;",
                Font_size_ <<- "4"
              ]
              $ toElement label
          )
            <> line_
              [ X1_ <<- (toText 50),
                Y1_ <<- (toText yPos),
                X2_ <<- (toText (8640 + 50)),
                Y2_ <<- (toText yPos),
                Stroke_ <<- "#C0C0C0",
                Stroke_width_ <<- "0.5"
              ]
        )
  )
    <> yLegend fy stations

-- | This code is never used, but maybe when
-- https://github.com/fosskers/streaming-osm/issues/3 is resolved?
extractReferenceTrackCached :: IO [ReferenceTrack]
extractReferenceTrackCached =
  let dataPath = "raw/brandenburg-latest.osm.pbf"
      cachePath = "cache/reference-tracks.json"
      -- filter through all relations so we get the one we need
      filterRelations :: Relation -> Bool
      filterRelations (Relation {_rinfo = Nothing}) = False
      filterRelations (Relation {_rinfo = Just (Info {_id = relationId})}) = relationId == 178663 -- Tram96
      filterWays :: [Int] -> Way -> Bool
      filterWays ids (Way {_winfo = Nothing}) = False
      filterWays ids (Way {_winfo = Just (Info {_id = wayId})}) = P.elem wayId ids
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

days :: [String]
days =
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

styleElement :: Element
styleElement = style_ [] "path { mix-blend-mode: multiply; }"

main :: IO ()
main = do
  setLocaleEncoding utf8
  (referenceTrack96, stations96) <- readReferenceTrackFromFile
  linesOneDay <- getAllVehiclesCached ["2020-07-06"] filter96
  P.writeFile "2020-07-06_96.svg" $ P.show $ svg $ lineToElement "black" 1 stations96 referenceTrack96 linesOneDay
  lines <- getAllVehiclesCached days filter96
  P.writeFile "all_days_96.svg" $ P.show $ svg $ lineToElement "#cccccc" 4 stations96 referenceTrack96 lines
