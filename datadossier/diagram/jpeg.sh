#! /usr/bin/env bash

inkscape cache/all_days_96_diagram.svg -e cache/all_days_96_diagram.svg.png -d 300
convert cache/all_days_96_diagram.svg.png -background white -alpha remove -alpha off -colorspace Gray cache/all_days_96_diagram.svg.jpeg

inkscape cache/2020-07-06_96_diagram.svg -e cache/2020-07-06_96_diagram.svg.png -d 300
convert cache/2020-07-06_96_diagram.svg.png -background white -alpha remove -alpha off -colorspace Gray cache/2020-07-06_96_diagram.svg.jpeg

