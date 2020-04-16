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
              Cy_ <<- showF (- fontSize / 2),
              R_ <<- showF (dotSize / 2)
            ]
            <> ( text_
                   [ X_ <<- showF (textlvl + 13),
                     Font_size_ <<- showF fontSize
                   ]
                   $ (toElement name)
                     <> (toElement (". " :: Text))
                     <> ( (tspan_ [Style_ <<- "font-style:italic;"])
                            $ (toElement $ formatWeight weight)
                        )
               )

records :: [Record]
records =
  [ Record 0 00 "Dust particle (estimated)" 0.05,
    Record 0 00 "Crumpled recipe for cookies" 0.55,
    Record 1 40 "Body piercing in plastic bag" 0.62,
    Record 2 73 "Sticker 50mm×50mm" 0.7,
    Record 0 00 "Sticker 84mm×55mm" 1.3,
    Record 2 76 "WiFi adapter" 2.5,
    Record 1 30 "Easter greetings card from my mother" 2.7,
    Record 0 00 "Protective mask" 2.9,
    Record 0 00 "Zine" 4.4,
    Record 1 18 "Cheat sheet for keyboard layout" 5.2,
    Record 2 59 "Easter greetings card from my father" 5.6,
    Record 0 00 "Ballpoint" 9,
    Record 3 93 "Cover of a precision balance, red" 9.44,
    Record 2 52 "Cardboard box for RaspberryPi" 10,
    Record 1 20 "Letter about tax return" 9.5,
    Record 0 00 "Square" 12.3,
    Record 0 00 "Lipstick" 18.4,
    Record 1 20 "Head set" 19.6,
    Record 2 36 "Nail scissors" 20.1,
    Record 3 51 "USB cable for keyboard" 25.4,
    Record 0 00 "Needled felt mammoth" 31.5,
    Record 1 34 "USB extension cord" 38.3,
    Record 0 00 "RaspberryPi 3B+" 42.7,
    Record 1 28 "HDMI cable" 52.7,
    Record 0 00 "Precision balance, red" 64.29,
    Record 2 51 "Scissors" 79.2,
    Record 1 33 "VGA cable" 80,
    Record 0 00 "Duct tape" 115.7,
    Record 1 21 "Roll of toilet paper" 130.7,
    Record 2 45 "Computer mouse" 140,
    Record 0 00 "Power adaptor for a monitor" 180,
    Record 1 40 "Power cord" 200,
    Record 0 00 "Ethernet switch" 240,
    Record 0 00 "Book of friendship from my flat mate, still missing a page about me" 380,
    Record 1 80 "Notebook" 410,
    Record 2 98 "Water glass, near empty" 450,
    Record 0 00 "Water glass, half-full" 520,
    Record 1 31 "Keyboard" 680,
    Record 0 00 "Small server, used as a monitor stand" 1550,
    Record 1 52 "Book about Bauhaus, used as a monitor stand" 1700,
    Record 0 00 "1st monitor" 3700,
    Record 1 24 "2nd monitor" 4400
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
formatWeight weight =
  let showF f = (pack $ show f)
      printfText fstr f = pack $ printf fstr f
   in printfText "%.1fg" weight

svg :: Element -> Element
svg content =
  doctype
    <> with
      (svg11_ content)
      [Version_ <<- "1.2", Width_ <<- "100mm", Height_ <<- "200mm"]

style :: Element
style =
  style_ [] $ toElement ("* { font: regular 5px sans-serif; }" :: Text)

title :: Element
title =
  text_
    [ Font_size_ <<- showF (3 * fontSize)
    ]
    $ toElement ("Weight distribution of things on my desk" :: Text)

diagram :: Element
diagram =
  g_
    [Transform_ <<- translate 40 150]
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
