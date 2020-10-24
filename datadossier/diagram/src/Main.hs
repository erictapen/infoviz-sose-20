{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Prelude as P
import GHC.IO.Encoding

import Diagram


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
