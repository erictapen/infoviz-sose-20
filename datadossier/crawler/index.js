'use strict'

const createHafas = require('vbb-hafas')
// const createThrottledHafas = require('./throttle')
// const createHafasWithRetry = require('./retry')

const hafas = createHafas('vbb-hafas-example')
// const hafas = createThrottledHafas('vbb-hafas-example', 5, 100)
// const hafas = createHafasWithRetry('vbb-hafas-example', {retries: 2})

const spichernstr = '900000042101'
const bismarckstr = '900000024201'

hafas.journeys(spichernstr, bismarckstr, {
	results: 1,
	tickets: true,
	stopovers: true,
	transferInfo: true
})
// .then(([journey]) => {
// 	return hafas.refreshJourney(journey.refreshToken, {transferInfo: true})
// })

// hafas.journeys({
// 	type: 'location',
// 	id: '900981377',
// 	name: 'Berlin, HTW-Berlin Campus Wilhelminenhof',
// 	latitude: 52.458359,
// 	longitude: 13.526635
// }, '900000192001', {results: 1})
// hafas.departures('900000013102', {duration: 1})
// hafas.locations('Alexanderplatz', {results: 2})
// hafas.stop('900000017104')
// hafas.nearby({
// 	type: 'location',
// 	latitude: 52.5137344,
// 	longitude: 13.4744798
// }, {distance: 60})
// hafas.radar({
// 	north: 52.52411,
// 	west: 13.41002,
// 	south: 52.51942,
// 	east: 13.41709
// }, {results: 10})

.then((data) => {
	console.log(require('util').inspect(data, {depth: null, colors: true}))
})
.catch((err) => {
	console.error(err)
	process.exitCode = 1
})
