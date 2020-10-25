{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Diagram
  ( diagramHeight,
    placeOnX,
    placeOnY,
    diagramCached,
    Diagram (Diagram),
  )
where

import Data.Maybe
import Data.Text as TS
import Data.Time.LocalTime
import Geo
import Graphics.Svg
import Hafas
import ReferenceTrack
import System.Directory
import System.Process
import Prelude as P

-- | This factor is used to calculate mm height in real y axis from m height in physical track length.
diagramHeightFactor :: Double
diagramHeightFactor = 0.02

-- | Height of the diagram. It is computed from the length of a ReferenceTrack, as we show absolute values.
diagramHeight :: ReferenceTrack -> Double
diagramHeight refTrack = (*) diagramHeightFactor $ fst $ P.last refTrack

-- | Construct a SVG document from a height and the content Element.
svg :: Double -> Text -> Element -> Element
svg width height content =
  doctype
    <> Graphics.Svg.with
      (svg11_ content)
      [ Version_ <<- "1.1",
        Width_ <<- (toText width),
        Height_ <<- height,
        ViewBox_ <<- "0 0 " <> (toText width) <> " " <> height
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

-- | Place an x value (which is a time stamp) on the x-axis. 10 seconds amount to one pixel.
placeOnX :: Double -> TimeOfDay -> Double
placeOnX width t = (seconds t) * (width / (3600 * 24))

-- | Try to place data on the y-axis. Needs a tolerance value and a ReferenceTrack.
placeOnY :: Double -> ReferenceTrack -> GeoCoord -> Maybe Double
placeOnY tolerance refTrack v = fmap (diagramHeightFactor *) $ locateCoordOnTrackLength tolerance refTrack v

-- | Diagram metadata.
data Diagram = Diagram Text FilePath Double Text (Maybe Double) ReferenceTrack [String]

-- | Transforms some metadata and a Line to an SVG ELement.
diagram :: Diagram -> [Hafas.Line] -> Element
diagram (Diagram _ _ width color strokeWidth refTrack dataFiles) lines =
  svg width (toText $ diagramHeight refTrack) $
    (style_ [] "path { mix-blend-mode: multiply; }")
      <> ( P.mconcat $
             P.map
               ( \(Hafas.Line _ trips) ->
                   P.mconcat
                     $ P.map
                       ( tripToElement
                           color
                           (fromMaybe (width / (6 * 60 * 24)) strokeWidth)
                           (placeOnX width . localTimeOfDay)
                           (placeOnY 10 refTrack)
                       )
                     $ trips
               )
               lines
         )

-- | Effectful and caching version of diagram.
diagramCached :: Diagram -> IO ()
diagramCached diagramData@(Diagram tramId outFile _ _ _ _ dataFiles) =
  do
    fileExists <- doesFileExist outFile
    if fileExists
      then P.putStrLn $ "Cache hit:  " <> outFile
      else do
        P.putStrLn $ "Cache miss: " <> outFile
        lines <- getAllVehiclesCached dataFiles $ filterTram tramId
        P.writeFile outFile $ P.show $ diagram diagramData lines
        readProcess "./raster.sh" [outFile] ""
        mempty
