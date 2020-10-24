{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Diagram
  ( diagramWidth,
    diagramHeight,
    placeOnX,
    placeOnY,
    diagramCached,
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

