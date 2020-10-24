{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Geo
  ( Meter,
    earthRadius,
    GeoCoord,
    distance,
    mapToTrack,
  )
where

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
import System.Process
import Prelude as P

type Meter = Double

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
-- mesurement. This function maps C to the track and returns the mapped distance
-- we treveled from A in meters.
-- Example:
-- mapToTrack (52.415193, 13.050288) (52.415795, 13.050324) (52.415283, 13.050306)
-- This should Just be about 10.3 meters.
mapToTrack :: Double -> GeoCoord -> GeoCoord -> GeoCoord -> Maybe Double
mapToTrack tolerance a b c =
  let (abx, aby, abz) = vecSubtract (geoToVec a) (geoToVec b)
      (acx, acy, acz) = vecSubtract (geoToVec a) (geoToVec c)
      abLength = vecLength (abx, aby, abz)
      res = (abx * acx + aby * acy + abz * acz) / abLength
   in -- We only accpt the mapping if it is on the track segment or at least
      -- within a specified amount of meters of it.
      if (- tolerance) <= res && res <= (abLength + tolerance)
        then Just res
        else Nothing
