-- SPDX-FileCopyrightText: 2020 Kerstin Humm <mail@erictapen.name>
--
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Geo
  ( Meter,
    earthRadius,
    GeoCoord,
    distance,
    mapToTrack,
    geoToTrackPoint,
    geoToPoint,
    TrackPoint (TrackPoint),
  )
where

import Data.Geospatial
import Data.Trees.KdTree
import Prelude as P

type Meter = Double

-- | Earth radius in meters
earthRadius :: Double
earthRadius = 6371000

-- | Geo coordinate
type GeoCoord = (Latitude, Longitude)

-- | Project a point on earths surface from geocoordinates to a 3D position in meters.
geoToPoint :: GeoCoord -> Point3d
geoToPoint (lat, lon) =
  let latR = lat * pi / 180
      lonR = lon * pi / 180
   in Point3d
        (earthRadius * cos lonR * cos latR)
        (earthRadius * sin lonR * cos latR)
        (earthRadius * sin latR)

data TrackPoint = TrackPoint Point3d Meter
  deriving (Eq)

instance Point TrackPoint where
  dimension (TrackPoint p _) = dimension p
  coord i (TrackPoint p _) = coord i p
  dist2 (TrackPoint p1 _) (TrackPoint p2 _) = dist2 p1 p2

-- | Turn a track mark and a coordinate into a TrackPoint, so we can insert it into a KdTree.
geoToTrackPoint :: Meter -> GeoCoord -> TrackPoint
geoToTrackPoint mark geo = TrackPoint (geoToPoint geo) mark

-- | Get the length of a 3D vector
-- Example:
-- vecLength $ vecSubtract (geoToPoint 52.359792 13.137204) (geoToPoint 52.424919 13.053749)
vecLength :: Point3d -> Double
vecLength (Point3d x y z) = sqrt $ x * x + y * y + z * z

-- | vecSubtract a b = vector from a to b (or ab)
vecSubtract :: Point3d -> Point3d -> Point3d
vecSubtract (Point3d x1 y1 z1) (Point3d x2 y2 z2) = Point3d (x2 - x1) (y2 - y1) (z2 - z1)

-- | Distance in meters between two Point3d.
distance :: Point3d -> Point3d -> Meter
distance a b = vecLength $ vecSubtract a b

-- | A and B are two connected Locations on the exact track. C is our inexact
-- mesurement. This function maps C to the track and returns the mapped distance
-- we treveled from A in meters.
-- Example:
-- mapToTrack (52.415193, 13.050288) (52.415795, 13.050324) (52.415283, 13.050306)
-- This should Just be about 10.3 meters.
mapToTrack :: Double -> Point3d -> Point3d -> Point3d -> Maybe Double
mapToTrack tolerance a b c =
  let ab@(Point3d abx aby abz) = vecSubtract a b
      (Point3d acx acy acz) = vecSubtract a c
      abLength = vecLength ab
      res = (abx * acx + aby * acy + abz * acz) / abLength
   in -- We only accpt the mapping if it is on the track segment or at least
      -- within a specified amount of meters of it.
      if (- tolerance) <= res && res <= (abLength + tolerance)
        then Just res
        else Nothing