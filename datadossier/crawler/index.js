// SPDX-FileCopyrightText: 2020 Justin Humm <mail@erictapen.name>
//
// SPDX-License-Identifier: GPL-3.0-or-later

'use strict'

const createHafas = require('vbb-hafas')

const hafas = createHafas('vbb-hafas-example')

hafas.radar({
  north: 52.4330,
  west: 13.0095,
  south: 52.3562,
  east: 13.1436
}, {results: 265})
.then((data) => {
  console.log(JSON.stringify(data))
})
.catch((err) => {
  console.error(err)
  process.exitCode = 1
})