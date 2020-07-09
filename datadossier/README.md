# Tram delays in Potsdam

## Building this datadossier by yourself

Unfortunately, this project isn't reproducible yet. Coming soon!


## How this works.

### Fetching the data

The Trams in Potsdam are operated by the [*Verkehrbetrieb Potsdam (ViP)*](https://www.swp-potsdam.de/de/verkehr/) which is a part of [*Verkehrsverbund Berlin-Brandenburg (VBB)*](https://www.vbb.de/). ViP as well as VBB don't offer an open API for getting delay data on their schedules, but the VBB internally uses a system called HAFAS for managing their train and bus schedules. HAFAS is very popular among european transport providers, so some kind souls wrote a [reverse engineered client](https://github.com/public-transport/hafas-client) for the system.

This `hafas-client` project is written in NodeJS and allows for querying the location of *all vehicles within a certain geographic window*.

```js
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
```

```json
  {
    "direction": "S Griebnitzsee",
    "tripId": "1|53338|1|86|20062020",
    "trip": 53338,
    "line": {
      "type": "line",
      "id": "616",
      "fahrtNr": null,
      "name": "616",
      "public": true,
      "class": 8,
      "mode": "bus",
      "product": "bus",
      "symbol": null,
      "nr": 616,
      "metro": false,
      "express": false,
      "night": false
    },
    "location": {
      "type": "location",
      "latitude": 52.403408,
      "longitude": 13.100438
    },
...
```

For course participants: See [`justin-example.json`](https://infovis.fh-potsdam.de/datasets/justin-example.json) on the SFTP server as an example.

I did that every 10 seconds for the course of the last two weeks.

## Processing the data

For processing the data I choose Haskell as a programming language. It allows for strong typing, which I find always convenient in building data pipelines, is comparatively fast and is [excellently suited to build SVG structures](http://hackage.haskell.org/package/svg-builder), which I want to use to construct my final graphic.

All the Haskell code is located in [`Main.hs`](extract-from-raw/src/Main.hs).

`TODO`: Explain the processing in more depth, will probably take the most part of the article.


## Visualizing the data

In this project I wanted to try a way of visualizing train delays I'd thought about for a long time. I very much like this historic schedule by *E. J. Marey*:

![](marey.jpg)

But this is a schedule that doesn't show the data from actual train rides. What if it could show data from actual rides? This way one could observe delay characteristics of a certain ride (e.g. 6.31 in the morning on work days) by looking at the sharpness of the line!

Here is an early example, showing the Tram line 96 on the 24th of June 2020.

![](preview.png)

See alse the generated SVG graphic [`preview.svg`](preview.svg).
