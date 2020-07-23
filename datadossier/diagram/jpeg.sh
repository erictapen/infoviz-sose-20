#! /usr/bin/env bash

inkscape "$1" -e "$1.png" -d 300
convert "$1.png" -background white -alpha remove -alpha off -colorspace Gray "$1.jpeg"

