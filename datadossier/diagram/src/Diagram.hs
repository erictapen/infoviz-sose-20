-- SPDX-FileCopyrightText: 2020 Kerstin Humm <mail@erictapen.name>
--
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Diagram
  ( placeOnX,
    placeOnY,
    diagramCached,
    Diagram (Diagram),
  )
where

import Data.Maybe
import Data.Text as TS
import Data.Time.LocalTime
import Data.Trees.KdTree
import Geo
import Graphics.Svg
import Hafas
import ReferenceTrack
import System.Directory
import System.Process
import Prelude as P

-- | Construct a SVG document from a height and the content Element.
svg :: Double -> Text -> Element -> Element
svg width height content =
  doctype
    <> Graphics.Svg.with
      (svg11_ content)
      [ Version_ <<- "1.1",
        Width_ <<- (toText width <> "mm"),
        Height_ <<- (height <> "mm"),
        ViewBox_ <<- "0 0 " <> (toText width) <> " " <> height
      ]

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
      [ D_
          <<- ( mA (fx t) y
                  <> (tripToElement' (0.5 * strokeWidth) fx fy ((t, v) : tripData))
                  <> (tripToElement' (-0.5 * strokeWidth) fx fy $ P.reverse ((t, v) : tripData))
              ),
        Stroke_ <<- "None",
        Fill_ <<- color,
        Id_ <<- ((<>) "trip" $ TS.pack $ P.show tripId)
      ]

-- | Recursively generate the tail of the path elements.
tripToElement' ::
  Double ->
  (LocalTime -> Double) ->
  (GeoCoord -> Maybe Double) ->
  [(LocalTime, GeoCoord)] ->
  Text
tripToElement' _ _ _ [] = ""
tripToElement' offset fx fy ((t, v) : ds) = case (fy v) of
  Just y -> lA (offset + fx t) y <> tripToElement' offset fx fy ds
  Nothing -> tripToElement' offset fx fy ds

-- | Place an x value (which is a time stamp) on the x-axis. Width is the
-- overall width of the diagram in millimeters. The time is "warped" that means
-- that the day starts at 3:00 AM and ends at 3:00AM. This way we won't have
-- any cuts in tram schedules.
placeOnX :: Double -> TimeOfDay -> Double
placeOnX width t =
  let sec = (seconds t)
      warpedSeconds =
        if sec < (3600 * 3)
          then sec + (3600 * 21)
          else sec - (3600 * 3)
   in warpedSeconds * (width / (3600 * 24))

-- | Try to place data on the y-axis. Needs a tolerance value and a ReferenceTrack.
placeOnY :: Double -> Double -> KdTree TrackPoint -> GeoCoord -> Maybe Double
placeOnY heightFactor tolerance refTrackTree coord = fmap (heightFactor *) $ locateCoordOnTrackLength tolerance refTrackTree coord

-- | Diagram metadata.
data Diagram
  = Diagram
      Text
      FilePath
      Double
      Double
      Text
      (Maybe Double)
      ReferenceTrack
      (Maybe Day)

-- | Transforms some metadata and a Line to an SVG ELement.
diagram :: Diagram -> Hafas.Line -> Element
diagram (Diagram _ _ width heightFactor color strokeWidth refTrack _) (Hafas.Line _ trips) =
  let refTrackTree :: KdTree TrackPoint
      refTrackTree = fromReferenceTrack refTrack
   in svg width (toText $ heightFactor * trackLength refTrack) $
        (style_ [] "path { mix-blend-mode: multiply; }")
          <> ( P.mconcat
                 $ P.map
                   ( tripToElement
                       color
                       (fromMaybe (width / (60 * 24)) strokeWidth)
                       (placeOnX width . localTimeOfDay)
                       (placeOnY heightFactor 10 refTrackTree)
                   )
                 $ trips
             )

-- | Effectful and caching version of diagram.
diagramCached :: Diagram -> IO ()
diagramCached diagramData@(Diagram tramId outFile _ _ _ _ _ day) =
  do
    fileExists <- doesFileExist outFile
    if fileExists
      then P.putStrLn $ "Cache hit:  " <> outFile
      else do
        P.putStrLn $ "Cache miss: " <> outFile
        line <- getAllVehiclesCached day $ filterTram tramId
        P.writeFile outFile $ P.show $ diagram diagramData line
        readProcess "./raster.sh" [outFile] ""
        mempty