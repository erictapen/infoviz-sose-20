-- SPDX-FileCopyrightText: 2020 Kerstin Humm <mail@erictapen.name>
--
-- SPDX-License-Identifier: GPL-3.0-or-later
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Svg where

import Graphics.Svg

-- document root
svg :: Double -> Double -> Element -> Element
svg height width content =
  doctype
    <> Graphics.Svg.with
      (svg11_ content)
      [ Version_ <<- "1.1",
        Height_ <<- (toText height <> "mm"),
        Width_ <<- (toText width <> "mm"),
        ViewBox_ <<- "0 0 " <> (toText width) <> " " <> (toText height)
      ]
