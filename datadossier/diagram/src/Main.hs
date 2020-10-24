{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Codec.Compression.GZip as GZ
import Control.Concurrent
import Control.DeepSeq
import Control.Monad.Trans.Resource
import Control.Parallel.Strategies
import Data.Aeson as Aeson
import Data.ByteString as BS
import Data.ByteString.Base64
import Data.ByteString.Lazy as BL
import Data.Functor
import Data.IntMap.Strict as IntMap
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Text as TS
import Data.Text.IO as TSIO
import Data.Time.LocalTime
import GHC.Generics
import GHC.IO.Encoding
import Geo
import Graphics.Svg
import Hafas
import ReferenceTrack
import Streaming.Osm
import Streaming.Osm.Types
import Streaming.Prelude as S
import System.Directory
import System.IO
import System.Process
import Prelude as P

-- | One pixel resolution for ten seconds, as we took samples this frequent.
diagramWidth :: Double
diagramWidth = 6 * 60 * 24

-- | This factor is used to calculate mm height in real y axis from m height in physical track length.
diagramHeightFactor :: Double
diagramHeightFactor = 0.02

-- | Height of the diagram. It is computed from the length of a ReferenceTrack, as we show absolute values.
diagramHeight :: ReferenceTrack -> Double
diagramHeight refTrack = (*) diagramHeightFactor $ fst $ P.last refTrack

-- document root
svg :: Text -> Text -> Element -> Element
svg height width content =
  doctype
    <> Graphics.Svg.with
      (svg11_ content)
      [ Version_ <<- "1.1",
        Width_ <<- width,
        Height_ <<- height,
        ViewBox_ <<- "0 0 " <> width <> " " <> height
      ]

svgInner :: Text -> Element -> Element
svgInner height content =
  doctype
    <> Graphics.Svg.with
      (svg11_ content)
      [ Version_ <<- "1.1",
        Width_ <<- (toText diagramWidth),
        Height_ <<- height,
        ViewBox_ <<- "0 0 " <> (toText diagramWidth) <> " " <> height
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
  Nothing -> mempty
  Just y ->
    circle_
      [ Cx_ <<- (toText $ fx t),
        Cy_ <<- (toText y),
        R_ <<- (toText $ 0.5 * strokeWidth),
        Stroke_ <<- "none",
        Fill_ <<- color
      ]
tripToElement color strokeWidth fx fy (tripId, (t, v) : tripData) = case (fy v) of
  Nothing -> tripToElement color strokeWidth fx fy (tripId, tripData)
  Just y ->
    path_
      [ D_ <<- (mA (fx t) y <> (tripToElement' fx fy tripData)),
        Stroke_ <<- color,
        Fill_ <<- "none",
        Stroke_width_ <<- (toText strokeWidth),
        Stroke_linecap_ <<- "round",
        Id_ <<- ((<>) "trip" $ TS.pack $ P.show tripId)
      ]

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

placeOnX :: TimeOfDay -> Double
placeOnX t = (*) 0.1 $ seconds t

placeOnY :: Double -> ReferenceTrack -> GeoCoord -> Maybe Double
placeOnY tolerance refTrack v = fmap (diagramHeightFactor *) $ locateCoordOnTrackLength tolerance refTrack v

-- | Transforms a Line to an SVG ELement.
diagramCached :: Text -> FilePath -> Text -> Double -> ReferenceTrack -> [String] -> IO ()
diagramCached tram outPath color strokeWidth referenceTrack days =
  let cachePath = outPath
      document :: [Hafas.Line] -> Element
      document lines =
        svgInner (toText $ diagramHeight referenceTrack) $
          (style_ [] "path { mix-blend-mode: multiply; }")
            <> ( P.mconcat $
                   P.map
                     ( \(Hafas.Line _ trips) ->
                         P.mconcat
                           $ P.map
                             ( tripToElement
                                 color
                                 strokeWidth
                                 (placeOnX . localTimeOfDay)
                                 (placeOnY 10 referenceTrack)
                             )
                           $ trips
                     )
                     lines
               )
   in do
        fileExists <- doesFileExist cachePath
        if fileExists
          then P.putStrLn $ "Cache hit:  " <> cachePath
          else do
            P.putStrLn $ "Cache miss: " <> cachePath
            lines <- getAllVehiclesCached days $ filterTram tram
            P.writeFile cachePath $ P.show $ document lines

instance NFData Element where
  rnf e = e `seq` ()

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
              [ X_ <<- (toText (fx t)),
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
              [ X1_ <<- (toText (fx t)),
                X2_ <<- (toText (fx t)),
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
      Nothing ->
        error $
          "Failed to map station "
            <> (TS.unpack label)
            <> ", "
            <> (P.show coord)
            <> " to y axis. Maybe increase tolerance?"
      (Just yPos) ->
        ( ( text_
              [ X_ <<- (toText (- 3)),
                Y_ <<- (toText (yPos + 1)),
                Font_family_ <<- "Fira Sans",
                Text_anchor_ <<- "end",
                Style_ <<- "text-align: end;",
                Font_size_ <<- "4"
              ]
              $ toElement label
          )
            <> line_
              [ X1_ <<- (toText 0),
                Y1_ <<- (toText yPos),
                X2_ <<- (toText (diagramWidth)),
                Y2_ <<- (toText yPos),
                Stroke_ <<- "#C0C0C0",
                Stroke_width_ <<- "0.5"
              ]
        )
  )
    <> yLegend fy stations

extractReferenceTrackCached :: IO [ReferenceTrack]
extractReferenceTrackCached =
  let dataPath = "./raw/brandenburg-latest.osm.pbf"
      cachePath = "./cache/reference-tracks.json"
      -- filter through all relations so we get the one we need
      filterRelations :: Relation -> Bool
      filterRelations (Relation {_rinfo = Nothing}) = False
      filterRelations (Relation {_rinfo = Just (Info {_id = relationId})}) = relationId == 178663 -- Tram96
      filterWays :: [Int] -> Way -> Bool
      filterWays ids (Way {_winfo = Nothing}) = False
      filterWays ids (Way {_winfo = Just (Info {_id = wayId})}) = P.elem wayId ids
      filterNodes :: [Int] -> Node -> Bool
      filterNodes ids (Node {_ninfo = Nothing}) = False
      filterNodes ids (Node {_ninfo = Just (Info {_id = nodeId})}) = P.elem nodeId ids
      osmToRefTrack :: [Node] -> ReferenceTrack
      osmToRefTrack [] = []
      osmToRefTrack (node : nodes) = (0, (_lat node, _lng node)) : (osmToRefTrack nodes)
   in do
        fileExists <- doesFileExist cachePath
        if fileExists
          then do
            TSIO.putStrLn $ "Cache hit:  " <> TS.pack cachePath
            cacheContent <- BL.readFile cachePath
            return $ fromJust $ Aeson.decode cacheContent
          else do
            TSIO.putStrLn $ "Cache miss: " <> TS.pack cachePath
            osmRelations <-
              runResourceT
                . S.head
                . S.filter filterRelations
                $ relations . blocks
                $ blobs dataPath
            TSIO.putStrLn $ TS.pack $ P.show osmRelations
            osmWays <-
              runResourceT
                . toList_
                . S.filter (filterWays $ P.map _mref $ _members $ fromJust $ fst' osmRelations)
                $ ways . blocks
                $ blobs dataPath
            TSIO.putStrLn $ TS.pack $ P.show osmWays
            osmNodes <-
              runResourceT
                . toList_
                . S.filter (filterNodes $ P.concat $ P.map _nodeRefs osmWays)
                $ nodes . blocks
                $ blobs dataPath
            TSIO.putStrLn $ TS.pack $ P.show $ P.length osmNodes
            let referenceTrack = osmToRefTrack osmNodes
             in do
                  -- BL.writeFile cachePath $ Aeson.encode referenceTracks
                  return [referenceTrack]

-- | Show a line as an CSV table, just a helper function I'll not use often.
printCSV :: Hafas.Line -> String
printCSV (Hafas.Line _ trips) =
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

data WebOrPrint = Web | Print

graphicWithLegendsCached :: String -> FilePath -> Text -> Double -> [String] -> WebOrPrint -> IO ()
graphicWithLegendsCached tram outFile color strokeWidth days webOrPrint =
  let cachePath =
        "./cache/" <> outFile
          <> ( case webOrPrint of
                 Web -> ""
                 Print -> "_print"
             )
          <> ".svg"
      diagramPath = "./cache/" <> outFile <> "_diagram.svg"
   in do
        fileExists <- doesFileExist cachePath
        if fileExists
          then P.putStrLn $ "Cache hit:  " <> cachePath
          else do
            P.putStrLn $ "Cache miss: " <> cachePath
            (refTrack, stations) <- readReferenceTrackFromFile $ tram <> ".json"
            diagramCached (TS.pack tram) diagramPath color strokeWidth refTrack days
            readProcess "./raster.sh" [diagramPath] ""
            rasterContent <- case webOrPrint of
              Web -> BS.readFile $ diagramPath <> ".jpeg"
              Print -> BS.readFile $ diagramPath <> ".png"
            P.writeFile cachePath
              $ P.show
              $ svg (toText $ (+) (40 + 40) $ diagramHeight refTrack) (toText $ diagramWidth + 100 + 20 + 20)
              $ g_
                [ Transform_ <<- translate 100 20
                ]
              $ let legend = (yLegend (placeOnY 100 refTrack) stations) <> (xLegend placeOnX)
                    image =
                      ( image_
                          [ X_ <<- (toText 0),
                            Y_ <<- (toText 0),
                            Width_ <<- (toText diagramWidth),
                            Height_ <<- (toText $ diagramHeight refTrack),
                            XlinkHref_ <<- case webOrPrint of
                              Web -> ("data:image/jpeg;base64," <> encodeBase64 rasterContent)
                              Print -> ("data:image/png;base64," <> encodeBase64 rasterContent)
                          ]
                      )
                 in -- For Print we use the transparent Png image, so we can put the legend behind it.
                    case webOrPrint of
                      Web -> image <> legend
                      Print -> legend <> image

plakat :: IO ()
plakat = do
  (rt91, _) <- readReferenceTrackFromFile "91.json"
  (rt92, _) <- readReferenceTrackFromFile "92.json"
  (rt93, _) <- readReferenceTrackFromFile "93.json"
  (rt94, _) <- readReferenceTrackFromFile "94.json"
  (rt96, _) <- readReferenceTrackFromFile "96.json"
  (rt98, _) <- readReferenceTrackFromFile "98.json"
  (rt99, _) <- readReferenceTrackFromFile "99.json"
  let heights = P.map diagramHeight [rt91, rt92, rt93, rt94, rt96, rt98, rt99]
      imagePaths =
        P.zip heights $
          [ "all_days_blended_91_diagram.svg.png",
            "all_days_blended_92_diagram.svg.png",
            "all_days_blended_93_diagram.svg.png",
            "all_days_blended_94_diagram.svg.png",
            "all_days_blended_96_diagram.svg.png",
            "all_days_blended_98_diagram.svg.png",
            "all_days_blended_99_diagram.svg.png"
          ]
      diagrams :: Double -> [(Double, Text)] -> Element
      diagrams _ [] = mempty
      diagrams cursorHeight ((height, filePath) : rs) =
        ( image_
            [ X_ <<- toText 0,
              Y_ <<- toText cursorHeight,
              Width_ <<- toText diagramWidth,
              Height_ <<- toText height,
              XlinkHref_ <<- filePath
            ]
        )
          <> diagrams (cursorHeight + height + (0.25 * 594 / 8)) rs
   in do
        P.print $ P.show heights
        P.writeFile "./cache/plakat.svg" $ P.show
          $ svg "594" "841"
          $ g_
            [ Transform_ <<- translate 100 20
            ]
          $ diagrams 0 imagePaths

main :: IO ()
main = do
  setLocaleEncoding utf8
  graphicWithLegendsCached "96" "2020-07-06_96" "black" 1 ["2020-07-06"] Web
  graphicWithLegendsCached "96" "all_days_96" "black" 1 allDays Web
  graphicWithLegendsCached "91" "all_days_blended_91" "#cccccc" 4 allDays Web
  graphicWithLegendsCached "92" "all_days_blended_92" "#cccccc" 4 allDays Web
  graphicWithLegendsCached "93" "all_days_blended_93" "#cccccc" 4 allDays Web
  graphicWithLegendsCached "94" "all_days_blended_94" "#cccccc" 4 allDays Web
  graphicWithLegendsCached "96" "all_days_blended_96" "#cccccc" 4 allDays Web
  graphicWithLegendsCached "98" "all_days_blended_98" "#cccccc" 4 allDays Web
  graphicWithLegendsCached "99" "all_days_blended_99" "#cccccc" 4 allDays Web
  graphicWithLegendsCached "91" "all_days_blended_91" "#cccccc" 4 allDays Print
  graphicWithLegendsCached "92" "all_days_blended_92" "#cccccc" 4 allDays Print
  graphicWithLegendsCached "93" "all_days_blended_93" "#cccccc" 4 allDays Print
  graphicWithLegendsCached "94" "all_days_blended_94" "#cccccc" 4 allDays Print
  graphicWithLegendsCached "96" "all_days_blended_96" "#cccccc" 4 allDays Print
  graphicWithLegendsCached "98" "all_days_blended_98" "#cccccc" 4 allDays Print
  graphicWithLegendsCached "99" "all_days_blended_99" "#cccccc" 4 allDays Print
  plakat
