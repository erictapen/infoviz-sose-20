{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

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
import System.Process
import Prelude as P

data WebOrPrint = Web | Print

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
