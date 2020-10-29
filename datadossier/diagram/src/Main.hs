{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.ParallelIO.Global
import Data.ByteString as BS
import Data.ByteString.Base64
import Data.Maybe
import Data.Text as TS
import Data.Time.LocalTime
import Diagram
import GHC.IO.Encoding
import Geo
import Graphics.Svg
import ReferenceTrack
import System.Directory
import Prelude as P

formatTime :: TimeOfDay -> Text
formatTime t =
  TS.pack $
    (P.show $ todHour t)
      <> ":"
      <> ( case (todMin t) of
             0 -> "00"
             _ -> (P.show (todMin t))
         )

xLegend :: Double -> (TimeOfDay -> Double) -> [TimeOfDay] -> Element
xLegend height fx times =
  P.mconcat $
    P.map
      ( \t ->
          ( text_
              [ X_ <<- (toText (fx t)),
                Y_ <<- (toText (height - 5)),
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
                Y1_ <<- (toText (height -3)),
                Y2_ <<- (toText (height -1)),
                Stroke_ <<- "black",
                Stroke_width_ <<- "1"
              ]
      )
      times

yLegend :: Double -> (GeoCoord -> Maybe Double) -> [(Text, GeoCoord)] -> Element
yLegend _ _ [] = mempty
yLegend width fy ((label, coord) : stations) =
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
                X2_ <<- (toText width),
                Y2_ <<- (toText yPos),
                Stroke_ <<- "#C0C0C0",
                Stroke_width_ <<- "0.5"
              ]
        )
  )
    <> yLegend width fy stations

-- document root
svg :: Double -> Double -> Element -> Element
svg height width content =
  doctype
    <> Graphics.Svg.with
      (svg11_ content)
      [ Version_ <<- "1.1",
        Width_ <<- (toText width <> "mm"),
        Height_ <<- (toText height <> "mm"),
        ViewBox_ <<- "0 0 " <> (toText width) <> " " <> (toText height)
      ]

-- | A graphic is a finished SVG file that can be presented e.g. in the
-- datadossier. It consists of a diagram and legends of x- and y-axis.
graphicWithLegendsCached :: String -> FilePath -> Text -> (Maybe Double) -> [String] -> IO ()
graphicWithLegendsCached tram outFile color strokeWidth days =
  let cachePath = "./cache/" <> outFile <> ".svg"
      diagramPath = "./cache/" <> outFile <> "_diagram.svg"
      diagramWidth = 6 * 6 * 24 -- One unit for 10 seconds
      diagramHeightFactor = 0.002
   in do
        fileExists <- doesFileExist cachePath
        if fileExists
          then P.putStrLn $ "Cache hit:  " <> cachePath
          else do
            P.putStrLn $ "Cache miss: " <> cachePath
            (refTrack, stations) <- readReferenceTrackFromFile $ tram <> ".json"
            diagramCached
              ( Diagram
                  (TS.pack tram)
                  diagramPath
                  diagramWidth
                  diagramHeightFactor
                  color
                  strokeWidth
                  refTrack
                  days
              )
            rasterContent <- BS.readFile $ diagramPath <> ".jpeg"
            P.writeFile cachePath
              $ P.show
              $ svg (40 + 40 + trackLength refTrack) (diagramWidth + 100 + 20 + 20)
              $ g_
                [ Transform_ <<- translate 100 20
                ]
              $ ( image_
                    [ X_ <<- (toText 0),
                      Y_ <<- (toText 0),
                      Width_ <<- (toText diagramWidth),
                      Height_ <<- (toText $ trackLength refTrack),
                      XlinkHref_ <<- ("data:image/jpeg;base64," <> encodeBase64 rasterContent)
                    ]
                )
                <> (yLegend diagramWidth (placeOnY diagramHeightFactor 100 $ fromReferenceTrack refTrack) stations)
                <> ( xLegend
                       0
                       (placeOnX diagramWidth)
                       [(TimeOfDay h m 0) | h <- [0 .. 23], m <- [0, 10 .. 50]]
                   )

tramIds :: [Text]
tramIds =
  [ "91",
    "92",
    "93",
    "94",
    "96",
    "98",
    "99"
  ]

plakat :: IO ()
plakat =
  let diagramWidth = 600
   in do
        referenceTracksAndStations <-
          parallel $
            P.map (\id -> fmap fst $ readReferenceTrackFromFile $ (TS.unpack id) <> ".json") tramIds
        let trackLengths = P.map trackLength referenceTracksAndStations
            totalTrackLength = sum trackLengths
            diagramHeightFactor = (0.75 * 500) / totalTrackLength
            gapSize = (0.25 * 500) / (fromIntegral $ P.length trackLengths - 1)
            imagePaths =
              P.zip
                (P.map (diagramHeightFactor *) trackLengths)
                $ P.map (\id -> "poster_diagram_" <> id <> ".svg.png") tramIds
            diagrams :: Double -> Double -> [(Double, Text)] -> Element
            diagrams _ _ [] = mempty
            diagrams gap cursorY ((height, filePath) : rs) =
              ( image_
                  [ X_ <<- (toText 0),
                    Y_ <<- (toText cursorY),
                    Width_ <<- (toText diagramWidth),
                    Height_ <<- (toText height),
                    XlinkHref_ <<- filePath
                  ]
              )
                <> ( xLegend
                       cursorY
                       (placeOnX diagramWidth)
                       [(TimeOfDay h m 0) | h <- [0 .. 23], m <- [0]]
                   )
                <> diagrams gap (cursorY + height + gap) rs
         in do
              parallel_
                $ P.map
                  ( \(tram, rt) ->
                      diagramCached
                        ( Diagram
                            tram
                            ("./cache/poster_diagram_" <> (TS.unpack tram) <> ".svg")
                            diagramWidth
                            diagramHeightFactor
                            "#cccccc"
                            Nothing
                            rt
                            allDays
                        )
                  )
                $ P.zip tramIds
                $ referenceTracksAndStations
              P.writeFile
                "./cache/plakat.svg"
                $ P.show
                $ svg 594 841
                $ g_
                  [ Transform_ <<- translate 100 20
                  ]
                $ diagrams gapSize 0 imagePaths

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

main :: IO ()
main = do
  setLocaleEncoding utf8
  graphicWithLegendsCached "96" "2020-07-06_96" "black" (Just 1) ["2020-07-06"]
  graphicWithLegendsCached "96" "all_days_96" "black" (Just 1) allDays
  graphicWithLegendsCached "91" "all_days_blended_91" "#cccccc" Nothing allDays
  graphicWithLegendsCached "92" "all_days_blended_92" "#cccccc" Nothing allDays
  graphicWithLegendsCached "93" "all_days_blended_93" "#cccccc" Nothing allDays
  graphicWithLegendsCached "94" "all_days_blended_94" "#cccccc" Nothing allDays
  graphicWithLegendsCached "96" "all_days_blended_96" "#cccccc" Nothing allDays
  graphicWithLegendsCached "98" "all_days_blended_98" "#cccccc" Nothing allDays
  graphicWithLegendsCached "99" "all_days_blended_99" "#cccccc" Nothing allDays
  plakat
