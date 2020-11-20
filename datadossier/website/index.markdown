<!--
SPDX-FileCopyrightText: 2020 Justin Humm <mail@erictapen.name>

SPDX-License-Identifier: GPL-3.0-or-later
-->

# Tram delays in Potsdam

In this datadossier I originally wanted to use geo data from trams in Potsdam, Germany, to visualize their delay characteristics. Unfortunately I made a big mistake in the process; I misinterpreted the nature of my data entirely. When I worked on this over the past months I repeatedly saw hints to my mistake, but I failed to fully acknowledge them, until two days before the deadline. So this is more a story about bad data interpretation and not so much one about tram delays, but of course it is also about tram delays.

## Fetching raw data

Trams in Potsdam are operated by the [*Verkehrbetrieb Potsdam (ViP)*](https://www.swp-potsdam.de/de/verkehr/) which is a part of [*Verkehrsverbund Berlin-Brandenburg (VBB)*](https://www.vbb.de/). ViP as well as VBB do not offer an open API for getting delay data on their schedules, but the VBB internally uses a system called HAFAS for managing their train and bus schedules. HAFAS is very popular among european transport providers, so some kind souls wrote a [reverse engineered client](https://github.com/public-transport/hafas-client) for the system.

This `hafas-client` project is written in NodeJS and provides multiple endpoints for querying the data from VBB. A good fit for my project would have been [`arrivals(station, [opt])`](https://github.com/public-transport/hafas-client/blob/5/docs/arrivals.md). It essentially receives a station ID and returns all incoming arrivals with their corresponding delay. The delays come only with one minute resolution, so basically one gets the content of a platform display.

As tram lines in Potsdam sometimes arrive as frequently as every 10 minutes, having only minute resolution would seriously restrict the insight one could have into delay characteristics. Another barrier would be, that one can only observe changes in delay between stations, but it's not possible to see what happens inbetween stations. So I ended up using the [`radar({north, west, south, east}, [opt])`](https://github.com/public-transport/hafas-client/blob/5/docs/radar.md) endpoint. As the name suggests, this endpoint provides a geocoordinate for every active public transport vehicle in a given area.

```js
'use strict'

const createHafas = require('vbb-hafas')

const hafas = createHafas('vbb-hafas-example')

hafas.radar({
  // the specified geowindow spans around all tram lines in Potsdam
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

With this geodata, I hoped to get delay information accurate to the second! As I'd know where a tram is and where its track is laid, I could determine the distance between where a tram should be according to schedule and where it actually is. That distance could then be used to accurately show the delay behind schedule.

As it turned out, this is where I was wrong. The geoinformation aquired by `radar` turned out to be not the actual location of the vehicles, but something else. More on that later.

As `radar` doesn't provide historic data, it was clear that I needed to fetch data continuously. In order to not overwhelm the VBB servers I needed to choose a fetching interval not too short, but short enough to make meaningful features visible. Features I was hoping to see are the amount of time a tram stops at a station or specific de-/acceleration behaviour, that happens always at the same spot, e.g. at traffic intersections. I measured once how long a typical tram stop lasts (23 seconds on crowded *Brandenburger Straße*) and concluded, that polling the API every 10 seconds would be sufficient.

To artifically increase the sampling rate I didn't query on every full 10 seconds of the day, but distributed the requests randomly over its 10 second interval. To illustrate this idea with an example: A tram 96 departs from *Platz der Einheit/West* everyday on `13:47:00` if it's on schedule. Measurung every day on `13:47:00` and `13:47:10` would give me only two samples about the acceleration behaviour of the tram in that interval. If I waited randomly between 0 and 10 seconds, I could get much more samples over the ideal acceleration, as one day I might sample `13:47:02` and on another day `13:47:03`, increasing the overall amount of samples I'm seeing.

To implement this requirements, I used Systemd timer. The service and timer definition below were generated by the [NixOS module in the repository](https://github.com/erictapen/infoviz-ss-20/blob/master/datadossier/crawler/module.nix). The resulting timer looks somewhat like this:

```sh
# /etc/systemd/system/vbb-crawler.timer
[Unit]
Description=VBB-Crawler

[Timer]
AccuracySec=1us
# Query every full 10 seconds
OnCalendar=*:*:0/10
# Wait evenly distributed between 0 and 10 seconds before actually
# starting the job.
RandomizedDelaySec=10s
Unit=vbb-crawler.service

```

```sh
# /etc/systemd/system/vbb-crawler.service
[Unit]
Description=VBB-Crawler

[Service]
...
# Path to the shell script that calls NodeJS. See
# https://github.com/erictapen/infoviz-ss-20/blob/master/flake.nix#L22
# for contents.
ExecStart=/nix/store/...-vbb-crawler.sh/bin/vbb-crawler.sh
Type=oneshot
WorkingDirectory=/var/lib/vbb-crawler
```

With that infrastructure in place I ran the crawler for 21 weekdays and accumulated around 2.7GiB of gzipped JSON data. That is too much to be published in this repository, but you can find an intermediate representation in the [`cache/` directory](https://github.com/erictapen/infoviz-ss-20/tree/master/datadossier/diagram/cache). This means that you could always invalidate the cache and use this project with your own data, but at the same time you are able to regenerate the diagrams from the exact same cache content they were generated from. More about reproducing this project [here](https://github.com/erictapen/infoviz-ss-20/tree/master/datadossier).


## Mapping vehicle locations to track positions

For determining the delay of a particular vehicle one needs to project the vehicle location onto the known track geometry, so one can derive statements like "this tram is at 70.6% of the track, which is around 13 seconds behind schedule".

As this operation would need to be applied to a lot of data and also might become quite complex, I chose [Haskell](http://haskell.org/) as the programming language for the task. Haskell is a fast and strongly typed functional programming language. I usually enjoy building data pipelines in Haskell, as due to the strong type system most of the errors are catched at compile time. This reduces the amount of times where I find my computation aborted, just due to an casting error in the last steps. Also I like the language in general and want to become better at it.

One thing I didn't like about my choice is that the Haskell ecosystem is not very developed in terms of consistency, accessability and availability of libraries for certain tasks. This e.g. resulted in quite a bit of conversion code in the final software or the fact that I had to use a small Rust program to read OpenStreetMap files, as the Haskell library for this task had a critical bug. Also I didn't receive the performance I hoped for. For sure there are lots of possible optimisation opportunities in my code, but I know too little about optimizing Haskell code to implement them.

All the data processing logic sits in [`src/Main.hs`](https://github.com/erictapen/infoviz-ss-20/blob/master/datadossier/diagram/src/Main.hs). In the following, some bits of the code are explained. `Main.hs` contains many comments, so if you want to understand it more deeply it might be worth a look.

`GeoCoord` is a pair of doubles, e.g. `(52.3980439, 13.0593256)`. `Vec` is a triple of doubles, representing the 3D position of one spot on the earths surface in meters. It's not important to which origin this vector relates, as we only look at relative distances between vectors.

```hs
type GeoCoord = (Double, Double)

type Vec = (Double, Double, Double)

earthRadius :: Double
earthRadius = 6371000

geoToVec :: GeoCoord -> Vec
geoToVec (lat, lon) =
  let latR = lat * pi / 180
      lonR = lon * pi / 180
   in ( earthRadius * cos lonR * cos latR,
        earthRadius * sin lonR * cos latR,
        earthRadius * sin latR
      )
```
Using `geoToVec` we can compute a `Vec` from a `GeoCoord`.

```hs
> geoToVec (52.3980439, 13.0593256)
(3786865.6860159975,878397.8341276174,5047544.603178312)
```

`vecLength` and `vecSubtract` are helper functions to determine the length of a vector and to substract one vector from another.

```hs
vecLength :: Vec -> Double
vecLength (x, y, z) = sqrt $ x * x + y * y + z * z

vecSubtract :: Vec -> Vec -> Vec
vecSubtract (x1, y1, z1) (x2, y2, z2) = (x2 - x1, y2 - y1, z2 - z1)
```

With these definitions we can define `mapToTrack`, a function that maps a `GeoCoord` `c` to a track segment, defined by two more `GeoCoord`s `a` and `b`.

```hs
mapToTrack :: GeoCoord -> GeoCoord -> GeoCoord -> Maybe Double
mapToTrack a b c =
  let (abx, aby, abz) = vecSubtract (geoToVec a) (geoToVec b)
      (acx, acy, acz) = vecSubtract (geoToVec a) (geoToVec c)
      abLength = vecLength (abx, aby, abz)
      res = (abx * acx + aby * acy + abz * acz) / abLength
   in if 0 <= res && res <= abLength
        then Just res
        else Nothing
```

The projection is done by computing the [scalar product](https://en.wikipedia.org/wiki/Dot_product#Scalar_projection_and_first_properties) between the vectors $\vec{ab}$ and $\vec{ac}$. Note that this computation can fail (thus having `Maybe` as return type), as we don't accept mappings that lie outside the track segment.

```hs
> mapToTrack (52.415193, 13.050288) (52.415795, 13.050324) (52.415283, 13.050306)
Just 10.0453908483592
```

`ReferenceTrack` is a list of pairs of a double (in meters) and a `GeoCoord`. It represents the actual track along which the tram travels and also contains somewhat of a [location marker](https://en.wikipedia.org/wiki/Highway_location_marker) for every coordinate, which tells us how many meters into the track a given coordinate is. We use the data structure to project a given tram location to the track and to determine how much of the overall track the tram has traveled.

```hs
type ReferenceTrack = [(Meter, GeoCoord)]
```

These are the first entries of the `ReferenceTrack` used for tram line 96, which was derived from [OpenStreetMap](https://www.openstreetmap.org/relation/178663) data. Have a look [here](https://github.com/erictapen/infoviz-ss-20/tree/master/datadossier/reference-tracks) for more infos on how I extracted the data.

```hs
[ (0.0,(52.357941999999994,13.1372282)),
  (3.465415935917142,(52.357973099999995,13.137231499999999)),
  (8.269271974171176,(52.358016299999996,13.1372322)),
  (61.69018202916258,(52.358496599999995,13.137214199999999)),
  (94.63257063748799,(52.358792799999996,13.137204599999999)),
...
]
```

With a `ReferenceTrack` and the `mapToTrack` function, we can finally determine how many meters a given tram has traveled on the whole track. The `locateCoordOnTrackLength` function might look a bit complicated, but it basically does the following:

- Determine the two points of the track that are nearest to our data point.
- Treat these two track points as a track segment.
- Project the data point onto that track segment using `mapToTrack`.
- If `mapToTrack` returns something, return that value plus the current location marker, normalized to the overall track length (14238 meters in the case of tram line 96). If not, return `Nothing`. This would mean that a mapping is not possible, as we obviously cannot project every possible location on earth to the track.

```hs
locateCoordOnTrackLength :: ReferenceTrack -> GeoCoord -> Maybe Double
locateCoordOnTrackLength track coord =
  let compareByDistance (_, a) (_, b) = compare (distance coord a) (distance coord b)
      compareByPosition (a, _) (b, _) = compare a b
      overallTrackLength = fst $ P.last track
      -- The two trackpoints closest to coord, sorted by their position on the track.
      twoClosestTrackPoints =
        sortBy compareByPosition
          $ P.take 2
          $ sortBy compareByDistance track
      (currentMark, firstPoint) = twoClosestTrackPoints !! 0
      (_, secondPoint) = twoClosestTrackPoints !! 1
   in case mapToTrack firstPoint secondPoint coord of
        (Just v) -> Just $ (currentMark + v) / overallTrackLength
        Nothing -> Nothing
```

## Visualizing the data

So far there is no sign of that false assumption about the data that I mentioned earlier. But it will become visible pretty soon.

From the initial idea on, I wanted to visualise the tram rides in a way similar to this historic graphic from 1885 by *E.J. Marey*, which shows a train schedule. The trains traversal through time and space is visualised with time on the x-axis and travelled distance on the y-axis. This way the train is visible as a continious line, that runs diagonally in times of motion and horizontally when the train is standing. One can easily determine speed and travel direction from the steepness of the lines.

<img style="max-width: 100%;" src="images/marey.jpg">

Of course this graphic only shows the planned schedule over the day. But it is easy to show actual trip data.

For generating graphics I also used Haskell, as there exists an [excellent library](http://hackage.haskell.org/package/svg-builder) for constructing SVG files. The `tripToElement` function transforms a bunch of inputs into a SVG `Element`. These inputs are

- `LocalTime -> Double`, a function to linearly position a timestamp on the x-axis.
- `GeoCoord -> Maybe Double`, a function that positions a coordinate on the y-axis, with the opportunity to fail. This will be `(locateCoordOnTrackLength referenceTrack96)`.
- `(TripId, [(LocalTime, GeoCoord)]`, basically a list of time and space data points, grouped together by a `TripId`, which is an `Int` found in the raw data and is used to group data points together.

```hs
tripToElement ::
  (LocalTime -> Double) ->
  (GeoCoord -> Maybe Double) ->
  (TripId, [(LocalTime, GeoCoord)]) ->
  Element
tripToElement _ _ (_, []) = mempty
-- For Trips with a single point we draw a circle instead of a path, as
-- otherwise the path wouldn't be visible.
tripToElement fx fy (_, (t, v) : []) = case (fy v) of
  Just y ->
    circle_
      [ Cx_ <<- (toText $ fx t),
        Cy_ <<- (toText y),
        R_ <<- "0.5",
        Stroke_ <<- "none",
        Fill_ <<- "black"
      ]
  Nothing -> mempty
tripToElement fx fy (tripId, (t, v) : tripData) = case (fy v) of
  Just y ->
    path_
      [ D_ <<- (mA (fx t) y <> (tripToElement' fx fy tripData)),
        Stroke_ <<- "black",
        Fill_ <<- "none",
        Stroke_width_ <<- "1",
        Stroke_linecap_ <<- "round",
        Id_ <<- ((<>) "trip" $ TS.pack $ P.show tripId)
      ]
  Nothing -> tripToElement fx fy (tripId, tripData)
```

Let's apply this to some fantasy data:

```hs
import Graphics.Svg
import Data.Text.IO as TSIO
import Data.Text.Lazy (toStrict)

main :: IO ()
main = do
  (referenceTrack96, _) <- readReferenceTrackFromFile
  TSIO.putStrLn
    $ Data.Text.Lazy.toStrict
    $ prettyText
    $ svg
    $ tripToElement
      (seconds . localTimeOfDay)
      (locateCoordOnTrackLength referenceTrack96)
      ( 0,
        [ (parseTime' "2020-07-09 12:00:05", (52.3584966, 13.1372142)),
          (parseTime' "2020-07-09 12:00:10", (52.3619684, 13.1370913))
        ]
      )

```

The code contains lots of conversion code, but essentially it applies `tripToElement` to some data. It results in a SVG path element with two nodes.

```svg
<path
  d="M 43205.0000,0.0043 L 43210.0000,0.0315 "
  stroke="black"
  stroke-width="1"
  fill="none"
  stroke-linecap="round"
  id="trip0"
/>
```

Soon we have everything to visualise all data from tram line 96 in *Marey* style diagram. To process only the data points that belong to a specific tram line, we introduce a `Filter` type. It is mainly a function that takes a pair of `LocalTime` and a `Vehicle` (one location measurement of one tram) and returns a `Bool` that signifies wether the data point is included or excluded in later analysis.

```hs
data Filter = Filter Text ((LocalTime, Vehicle) -> Bool)

instance Semigroup Filter where
  (Filter name1 f) <> (Filter name2 g) = Filter
    (name1 <> "-" <> name2)
    $ \x -> (f x) || (g x)
```

`filter96` is a `Filter` that only allows data points belonging to the tram line 96.

```hs
filter96 :: Filter
filter96 = Filter "filter96" $ \(_, v) -> tramId v == "96"
```

Putting everything we have together, we can finally show all data for a tram ride in one diagram. It's the tram line 96 for the 6th of July 2020. On the y-axis reside all stations from *Marie-Juchacz-Straße* to *Campus Jungfernsee* (with 14.2km track length inbetween) and on the x-axis the whole day from *00:00* to *23:59* is displayed.

<div>
  <div id="scroll-diagram"><img style="height: 30em" src="images/2020-07-06_96.svg"/>
  </div>
</div>

This is the moment where one could become suspicious of the data shown. First, we are seeing gaps inbetween lines. These gaps mostly are introduced by `splitTrip`, a function that I didn't explain yet. It splits continous lines, if they have gaps inbetween data points, that are more than 277m on the y axis apart. 277m is about the distance a Tram at 100km/h travels in 10 seconds, so this is in order to make anomalies in the data visible where its implausible that the tram actually traveled that fast.

But after closer inspection the data becomes even more implausible. Sometimes the lines jump back in space, so it appears like the tram traveled backwards for some hundred meters. While this may happen at some rare occasions (e.g. traffic accidents), it's very unlikely to happen this often during one day as we see it here.

So something must be wrong with the data. At first I suspected GPS inaccuracies. But the errors we are seeing are untypical for GPS; Most of the time the locations are very accurate on track and suddenly they jump around hundreds of meters. The system that aggregates the data could have weird behaviour regarding data processing, where some changes come in delayed for a while and suddenly "unclog" in a "hiccup". All these explanations aren't really satisfying, but in the development process I postponed finding a good answer and rather wrote Haskell code, as this was more fun. Until I managed to draw the data of multiple days ontop of each other. Then it became quiet obvious what was the reason behind the anomalies.

<div>
  <div id="scroll-diagram"><img style="height: 30em" src="images/all_days_96.svg"/>
  </div>
</div>

The coordinates I was processing never were actual GPS measurements in the first place. They are just the output of some system that knows about delay times within 60 second precision and **tries to derive the guessed location of the vehicle** from that information. At least this is the only possible explanation I can come up with for these patterns; A lot of parallel stripes, that are exactly 60 seconds apart on the y-axis.

So why on earth would an API deliver location data, that is not actual location data but can be derived from a much simpler endpoint of the same API? I don't know, but I guess this endpoint is designed so one can easily create an up-to-date war room style map of all active vehicles in the fleet. Maybe it even delivers actual location data for vehicles that support it and just fills in the gaps for everything that doesn't?

Anyway, it's quite annoying to find out about the true nature of the raw data this late into the project. This could have gotten a lot more technologically elegant; I could have used the `arrival` HAFAS endpoint in the first place, which would have resulted in much fewer requests and orders of magnitude fewer data. So what I'm taking away here once more, is that actually looking at the data is something that should be never postponed. I was so busy building my Haskell code towards an assumed goal, that I never questioned the necessary foundations of that goal.

In the end I'd say that this attempt still was a success. The delay information is the most accurate one can get, given that the HAFAS API is the only interface available. It might not have 1 second but 60 second precision, but it still allows for some neat insights. Especially when one blends the individual days into each other, so that delay distributions become visible.

<div>
  <div id="scroll-diagram"><img style="height: 30em" src="images/all_days_blended_96.svg"/>
  </div>
</div>