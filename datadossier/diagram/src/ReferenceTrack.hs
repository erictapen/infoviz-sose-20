{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReferenceTrack
  ( ReferenceTrack,
    readReferenceTrackFromFile,
    locateCoordOnTrackLength,
  )
where

import Data.Aeson as Aeson
import Data.ByteString.Lazy as BL
import Data.List
import Data.Maybe
import Data.Text as TS
import GHC.Generics
import Geo
import Graphics.Svg
import System.IO
import Prelude as P

type ReferenceTrack = [(Meter, GeoCoord)]

data ReferenceTrackJson
  = ReferenceTrackJson
      { label :: Text,
        coordinates :: [GeoCoord],
        stations :: [(Text, GeoCoord)]
      }
  deriving (Generic, Show)

instance FromJSON ReferenceTrackJson

-- | Openstreetmap doesn't have complete data about Potsdam train tracks and I'm too dumb to use their editor. So we manually add sone stuff here.
completeReferenceTrack :: ReferenceTrackJson -> ReferenceTrackJson
completeReferenceTrack rt@(ReferenceTrackJson label coordinates stations)
  -- Bad bad bad ugly hack. Tram 99 actually starts at Bisamkiez, not Platz der Einheit/Nord. So we have to subtract the first two stations (and their coordinates) and then append the nine more stations from Bisamkiez on.
  | (label == "99") =
    let bisamkiezToPdEBF :: [(Text, GeoCoord)]
        bisamkiezToPdEBF =
          [ ("Biesamkiez", (52.3733498, 13.1011966)),
            ("Magnus-Zeller-Platz", (52.3751684, 13.0912547)),
            ("Waldstraße/Horstweg", (52.3775220, 13.0829446)),
            ("Kunersdorfer Straße", (52.3805657, 13.0784618)),
            ("Sporthalle", (52.3826321, 13.0754337)),
            ("Friedhöfe", (52.3860868, 13.0715218)),
            ("S Potsdam Hauptbahnhof", (52.3910046, 13.0652694)),
            ("Lange Brücke", (52.3923948, 13.0635833)),
            ("Alter Markt/Landtag", (52.3951453, 13.0592313))
          ]
     in ReferenceTrackJson
          label
          ((P.map snd bisamkiezToPdEBF) ++ (P.drop 33 coordinates))
          (bisamkiezToPdEBF ++ (P.drop 2 stations))
  | otherwise = rt

-- | This returns the distance a tram has traveled, starting from the start of the track, in meters.
-- Shouldn't be < 0 or longer than the track, but I'm not sure!
locateCoordOnTrackLength :: Double -> ReferenceTrack -> GeoCoord -> Maybe Double
locateCoordOnTrackLength tolerance track coord =
  let compareByDistance (_, a) (_, b) = compare (distance coord a) (distance coord b)
      compareByPosition (a, _) (b, _) = compare a b
      -- The two trackpoints closest to coord, sorted by their position on the track.
      twoClosestTrackPoints =
        sortBy compareByPosition
          $ P.take 2
          $ sortBy compareByDistance track
      (currentMark, firstPoint) = twoClosestTrackPoints !! 0
      (_, secondPoint) = twoClosestTrackPoints !! 1
   in case mapToTrack tolerance firstPoint secondPoint coord of
        (Just v) -> Just $ (currentMark + v)
        Nothing -> Nothing

-- | Put the current kilometre mark on the track. Needs a starting Meter, as it
-- operates recursviely.
enrichTrackWithLength :: Meter -> [GeoCoord] -> ReferenceTrack
enrichTrackWithLength _ [] = []
enrichTrackWithLength m (x : []) = (m, x) : []
enrichTrackWithLength m (x : next : xs) =
  (m, x) : (enrichTrackWithLength (m + distance x next) (next : xs))

readReferenceTrackFromFile :: FilePath -> IO (ReferenceTrack, [(Text, GeoCoord)])
readReferenceTrackFromFile f = do
  fileContent <- BL.readFile $ "./cache/" <> f
  case (Aeson.eitherDecode fileContent) of
    (Right res) ->
      let completedRes = completeReferenceTrack res
       in return (enrichTrackWithLength 0 $ coordinates completedRes, stations completedRes)
    (Left err) -> error "Can't deserialise Referencetrack."