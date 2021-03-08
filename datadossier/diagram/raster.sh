#! /usr/bin/env bash

# SPDX-FileCopyrightText: 2020 Kerstin Humm <mail@erictapen.name>
#
# SPDX-License-Identifier: GPL-3.0-or-later

inkscape "$1" -e "$1.png" -d 300
convert "$1.png" -background white -alpha remove -alpha off -colorspace Gray "$1.jpeg"