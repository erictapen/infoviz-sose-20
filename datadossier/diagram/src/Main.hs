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
import Hafas
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
                Font_size_ <<- "3.5"
              ]
              $ toElement
              $ formatTime t
          )
            <> line_
              [ X1_ <<- (toText (fx t)),
                X2_ <<- (toText (fx t)),
                Y1_ <<- (toText (height -3)),
                Y2_ <<- (toText (height -1)),
                Stroke_ <<- "#C0C0C0",
                Stroke_width_ <<- "0.2"
              ]
      )
      times

yLegend :: Double -> Double -> (GeoCoord -> Maybe Double) -> [Station] -> Element
yLegend cursorY width fy stations =
  let indentedStations = P.zip stations $ cycle [False, True]
      yPos (label, coord) =
        maybe
          ( error $
              "Failed to map station "
                <> (TS.unpack label)
                <> ", "
                <> (P.show coord)
                <> " to y axis. Maybe increase tolerance?"
          )
          id
          $ fy coord
      legendText :: (Station, Bool) -> Element
      legendText (station@(label, _), indented) =
        ( text_
            [ X_ <<- (toText (- 3 - (if indented then 40 else 10))),
              Y_ <<- (toText (cursorY + (yPos station) + 1)),
              Font_family_ <<- "Fira Sans",
              Text_anchor_ <<- "end",
              Style_ <<- "text-align: end;",
              Font_size_ <<- "2"
            ]
            $ toElement label
        )
      legendLine :: (Station, Bool) -> Element
      legendLine (station, indented) =
        line_
          [ X1_ <<- (toText $ 0 - (if indented then 40 else 10)),
            Y1_ <<- (toText $ cursorY + (yPos station)),
            X2_ <<- (toText width),
            Y2_ <<- (toText $ cursorY + (yPos station)),
            Stroke_ <<- "#C0C0C0",
            Stroke_width_ <<- "0.2"
          ]
   in mconcat $
        (P.map legendLine indentedStations)
          <> P.map legendText indentedStations

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
graphicWithLegendsCached :: String -> FilePath -> Text -> (Maybe Double) -> Maybe Day -> IO ()
graphicWithLegendsCached tram outFile color strokeWidth day =
  let cachePath = "./cache/" <> outFile <> ".svg"
      diagramPath = "./cache/" <> outFile <> "_diagram.svg"
      diagramWidth = 6 * 6 * 24 -- One unit for 100 seconds
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
                  day
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
                <> (yLegend 0 diagramWidth (placeOnY diagramHeightFactor 100 $ fromReferenceTrack refTrack) stations)
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

-- | A big number in front of the diagram, signifying the Id of the tram.
tramIdHeading :: Double -> Text -> Element
tramIdHeading y id =
  let fontSize = 12
   in ( text_
          [ X_ <<- (toText $ -90),
            Y_ <<- (toText $ y + fontSize),
            Font_family_ <<- "Fira Sans",
            Font_weight_ <<- "Bold",
            Text_anchor_ <<- "middle",
            Style_ <<- "text-align: middle; font-weight: bold;",
            Font_size_ <<- toText fontSize
          ]
          $ toElement id
      )
        <> rect_
          [ X_ <<- (toText $ -90 - 0.5 * 1.38 * fontSize),
            Y_ <<- (toText $ y + 0 * fontSize),
            Height_ <<- (toText $ 1.20 * fontSize),
            Width_ <<- (toText $ 1.38 * fontSize),
            Ry_ <<- (toText $ fontSize / 4),
            Stroke_ <<- "black",
            Fill_ <<- "none",
            Stroke_width_ <<- (toText $ fontSize / 12)
          ]

plakat :: IO ()
plakat = do
  referenceTracksAndStations <-
    parallel $
      P.map (\id -> readReferenceTrackFromFile $ (TS.unpack id) <> ".json") tramIds
  let trackLengths = P.map trackLength $ P.map fst referenceTracksAndStations
      totalTrackLength = sum trackLengths
      diagramWidth = 660
      diagramHeight = 250
      diagramHeightFactor = (0.65 * diagramHeight) / totalTrackLength
      gapSize = (0.35 * diagramHeight) / (fromIntegral $ P.length trackLengths - 1)
      heights =
        P.zip tramIds $
          (P.map (diagramHeightFactor *) trackLengths)
      diagrams :: Double -> Double -> [(Text, Double)] -> [(ReferenceTrack, [Station])] -> Element
      diagrams _ _ [] _ = mempty
      diagrams gap cursorY ((tramId, height) : rs) ((refTrack, stations) : xs) =
        ( xLegend
            cursorY
            (placeOnX diagramWidth)
            [(TimeOfDay h m 0) | h <- [0 .. 23], m <- [0]]
        )
          <> (yLegend cursorY diagramWidth (placeOnY diagramHeightFactor 100 $ fromReferenceTrack refTrack) stations)
          <> ( image_
                 [ X_ <<- (toText 0),
                   Y_ <<- (toText cursorY),
                   Width_ <<- (toText diagramWidth),
                   Height_ <<- (toText height),
                   XlinkHref_ <<- "poster_diagram_" <> tramId <> ".svg.png"
                 ]
             )
          <> (tramIdHeading cursorY tramId)
          <> diagrams gap (cursorY + height + gap) rs xs
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
                      Nothing
                  )
            )
          $ P.zip tramIds
          $ P.map fst referenceTracksAndStations
        P.writeFile
          "./cache/plakat.svg"
          $ P.show
          $ svg 594 841
          $ g_
            [ Transform_ <<- translate 135 35
            ]
          $ diagrams gapSize 0 heights referenceTracksAndStations

main :: IO ()
main = do
  setLocaleEncoding utf8
  graphicWithLegendsCached "96" "2020-07-06_96" "black" (Just 1) (Just $ SomeDay "2020-07-06")
  graphicWithLegendsCached "96" "all_days_96" "black" (Just 1) Nothing
  graphicWithLegendsCached "91" "all_days_blended_91" "#cccccc" Nothing Nothing
  graphicWithLegendsCached "92" "all_days_blended_92" "#cccccc" Nothing Nothing
  graphicWithLegendsCached "93" "all_days_blended_93" "#cccccc" Nothing Nothing
  graphicWithLegendsCached "94" "all_days_blended_94" "#cccccc" Nothing Nothing
  graphicWithLegendsCached "96" "all_days_blended_96" "#cccccc" Nothing Nothing
  graphicWithLegendsCached "98" "all_days_blended_98" "#cccccc" Nothing Nothing
  graphicWithLegendsCached "99" "all_days_blended_99" "#cccccc" Nothing Nothing
  plakat
