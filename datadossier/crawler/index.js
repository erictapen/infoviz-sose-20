'use strict'

const createHafas = require('vbb-hafas')

const hafas = createHafas('vbb-hafas-example')

hafas.radar({
  north: 52.4255,
  west: 12.9893,
  south: 52.3831,
  east: 13.1050
}, {results: 265})
.then((data) => {
  console.log(JSON.stringify(data))
})
.catch((err) => {
  console.error(err)
  process.exitCode = 1
})
