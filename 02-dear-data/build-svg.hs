{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Graphics.Svg
import Text.Printf

factor :: Float
-- factor = 50.0
factor = 70.0

dotSize = 1.0

fontSize = 2

showF :: Float -> Text
showF r = (pack $ show r) <> "mm"

type Weight = Float

type DotLvl = Float

type TextLvl = Float

data Record = Record DotLvl TextLvl Text Weight

instance ToElement Record where
  toElement (Record dotlvl textlvl name weight) =
    let
     in g_
          [Transform_ <<- (translate 0 $ (*) factor $ logBase 10 weight)]
          $ circle_
            [ Cx_ <<- showF (5 + dotlvl * dotSize),
              R_ <<- showF (dotSize / 2)
            ]
            <> ( text_
                   [ X_ <<- showF (textlvl + 13),
                     Y_ <<- showF (fontSize / 2),
                     Font_size_ <<- showF fontSize
                   ]
                   $ (toElement name)
                     <> (toElement (". " :: Text))
                     <> ( (tspan_ [Style_ <<- "font-style:italic;"]) $
                            (toElement $ formatWeight weight)
                        )
               )

records :: [Record]
records =
  [ Record 0 000 "Dust particle" 0.05,
    Record 0 000 "Crumpled recipe for cookies" 0.55,
    Record 1 040 "Body piercing in plastic bag" 0.62,
    Record 2 083 "Sticker 50mm×50mm" 0.7,
    Record 0 000 "Sticker 84mm×55mm" 1.3,
    Record 2 086 "WiFi adapter" 2.5,
    Record 1 035 "Easter greetings card from my mother" 2.7,
    Record 0 000 "Protective mask" 2.9,
    Record 0 000 "Zine" 4.4,
    Record 1 018 "Cheat sheet for keyboard layout" 5.2,
    Record 2 059 "Easter greetings card from my father" 5.6,
    Record 0 000 "Ballpoint" 9,
    Record 3 103 "Cover of a precision balance, red" 9.44,
    Record 2 057 "Cardboard box for RaspberryPi" 10,
    Record 1 020 "Letter about tax return" 9.5,
    Record 0 000 "Square" 12.3,
    Record 0 000 "Lipstick" 18.4,
    Record 1 020 "Head set" 19.6,
    Record 2 046 "Nail scissors" 20.1,
    Record 3 071 "USB cable for keyboard" 25.4,
    Record 0 000 "Needled felt mammoth" 31.5,
    Record 1 034 "USB extension cord" 38.3,
    Record 0 000 "RaspberryPi 3B+" 42.7,
    Record 1 028 "HDMI cable" 52.7,
    Record 0 000 "Precision balance, red" 64.29,
    Record 2 051 "Scissors" 79.2,
    Record 1 033 "VGA cable" 80,
    Record 0 000 "Duct tape" 115.7,
    Record 1 021 "Roll of toilet paper" 130.7,
    Record 2 055 "Computer mouse" 140,
    Record 0 000 "Power adaptor for a monitor" 180,
    Record 1 040 "Power cord" 200,
    Record 0 000 "Ethernet switch" 240,
    Record 0 000 "Book of friendship from my flat mate, still missing a page about me" 380,
    Record 1 080 "Notebook" 410,
    Record 2 105 "Water glass, near empty" 450,
    Record 0 000 "Water glass, half-full" 520,
    Record 1 031 "Keyboard" 680,
    Record 0 000 "Small server, used as a monitor stand" 1550,
    Record 1 052 "Book about Bauhaus, used as a monitor stand" 1700,
    Record 0 000 "1st monitor" 3700,
    Record 1 024 "2nd monitor" 4400
  ]

scaleMarks :: [(Float, Text)]
scaleMarks =
  [ (0.01, "10mg"),
    (0.1, "100mg"),
    (1, "1g"),
    (10, "10g"),
    (100, "100g"),
    (1000, "1kg"),
    (10000, "10kg")
  ]

scale :: (Float, Text) -> Element
scale (val, label) =
  g_
    [Transform_ <<- (translate 0 $ (*) factor $ logBase 10 val)]
    $ ( text_
          [ X_ <<- showF (-1.5),
            Y_ <<- showF (0.55 * fontSize),
            Font_size_ <<- showF fontSize,
            Style_ <<- "text-anchor: end; text-align: end;"
          ]
          $ toElement label
      )
      <> rect_
        [ Width_ <<- showF 1.5,
          Height_ <<- showF 0.2
        ]

formatWeight :: Float -> Text
formatWeight weight
  | weight < 0.1 =
    printfText "~%dmg" (round $ weight * 1000 :: Int)
  | weight < 1 =
    printfText "%dmg" (round $ weight * 1000 :: Int)
  | weight < 10 =
    printfText "%.2fg" weight
  | weight < 100 =
    printfText "%.1fg" weight
  | weight < 1000 =
    printfText "%dg" (round weight :: Int)
  | weight >= 1000 =
    printfText "%.2fkg" (weight / 1000)
  | otherwise =
    printfText "%.2fg" weight
  where
    showF f = (pack $ show f)
    printfText fstr f = pack $ printf fstr f

svg :: Element -> Element
svg content =
  doctype
    <> with
      (svg11_ content)
      [Version_ <<- "1.2", Width_ <<- "195mm", Height_ <<- "160mm"]

style :: Element
style =
  style_ [] $ toElement ("* { font: regular 5px sans-serif; }" :: Text)

title :: Element
title =
  text_
    [ Font_size_ <<- showF (3 * fontSize),
            Style_ <<- "font-weight: bold;"
    ]
    $ toElement ("Weight distribution of things on my desk" :: Text)

diagram :: Element
diagram =
  g_
    [Transform_ <<- translate 40 190]
    $ (mconcat $ Prelude.map Main.scale scaleMarks)
      <> ( mconcat $
             Prelude.map toElement records
         )

main :: IO ()
main = do
  print $ svg
    $ g_
      [ Transform_ <<- translate 50 70
      ]
    $ title <> diagram
