#[macro_use]
extern crate serde;

use geo::prelude::*;
use geo::Point;
use osmpbfreader::objects::{Ref, RelationId};
use osmpbfreader::{OsmId, OsmObj, Way};
use std::collections::btree_map::BTreeMap;
use std::fs::File;

#[derive(Serialize, Debug)]
struct ReferenceTrack<'a> {
    label: &'a str,
    coordinates: Vec<(f64, f64)>,
    stations: Vec<(String, (f64, f64))>,
}

type Coordinate = (f64, f64);

type Station = (String, Coordinate);

/// Get the specified relation from objects, determine all the ways (which consist of coordinates)
/// and stations (nodes that have a string associated with them).
fn determine_coordinates_and_stations(
    objects: &BTreeMap<OsmId, OsmObj>,
    track_id: &OsmId,
) -> (Vec<Coordinate>, Vec<Station>) {
    let mut coordinates = Vec::new();
    let mut stations = Vec::new();
    if let OsmObj::Relation(relation) = objects.get(&track_id).unwrap() {
        for reference in &relation.refs {
            match reference {
                Ref {
                    member: OsmId::Way(id),
                    ..
                } => {
                    if let OsmObj::Way(Way { nodes, tags, .. }) =
                        objects.get(&OsmId::Way(*id)).unwrap()
                    {
                        if tags.contains("railway", "tram") {
                            for node_id in nodes {
                                if let OsmObj::Node(node) =
                                    objects.get(&OsmId::Node(*node_id)).unwrap()
                                {
                                    coordinates.push((node.lat(), node.lon()));
                                }
                            }
                        }
                    }
                }
                Ref {
                    member: OsmId::Node(id),
                    ..
                } => {
                    if let OsmObj::Node(node) = objects.get(&OsmId::Node(*id)).unwrap() {
                        if node.tags.contains("railway", "tram_stop") {
                            stations.push((
                                node.tags.get("name").unwrap().to_string(),
                                (node.lat(), node.lon()),
                            ));
                        }
                    }
                }
                _ => {}
            }
        }
    }
    (sort_coordinates(coordinates), stations)
}

/// OSM data is not always in the correct order, so we have to hop from coordinate to the next
/// nearest coordinate and very much hope that the resulting path is the actual track.
fn sort_coordinates(mut coordinates: Vec<Coordinate>) -> Vec<Coordinate> {
    let mut res = Vec::new();
    // At first we assume that the first coordinate is actually the start of the track.
    let (mut cursor_lat, mut cursor_lon): Coordinate = coordinates.remove(0);
    while !coordinates.is_empty() {
        let mut shortest_dist: (f64, usize) = (f64::MAX, 0);
        let cursor_point = Point::new(cursor_lon, cursor_lat);
        // Look at every remaining point and get the index with the smallest distance to
        // cursor.
        for i in 0..coordinates.len() {
            let (lat, lon): Coordinate = *coordinates.get(i).unwrap();
            let dist: f64 = cursor_point.geodesic_distance(&Point::new(lon, lat));
            if shortest_dist.0 > dist {
                shortest_dist = (dist, i);
                cursor_lat = lat;
                cursor_lon = lon;
            }
        }
        // If shortest_dist was altered we remove the coordinate and set the new cursor.
        // Otherwise break.
        if shortest_dist.0 < f64::MAX {
            res.push((cursor_lat, cursor_lon));
            coordinates.remove(shortest_dist.1);
        } else {
            break;
        }
    }

    // Every two neighbouring Way objects have one node as intersection, so we dedup.
    res.dedup();
    res
}

fn main() {
    let tracks: Vec<(&str, OsmId)> = vec![
        ("91", OsmId::Relation(RelationId(273891))),
        ("92", OsmId::Relation(RelationId(300929))),
        ("93", OsmId::Relation(RelationId(142135))),
        ("94", OsmId::Relation(RelationId(172570))),
        ("96", OsmId::Relation(RelationId(178663))),
        ("98", OsmId::Relation(RelationId(302038))),
        ("99", OsmId::Relation(RelationId(1585258))),
    ];

    let raw_file = File::open("./raw/brandenburg-latest.osm.pbf").unwrap();
    let mut pbf = osmpbfreader::OsmPbfReader::new(&raw_file);
    let objects = pbf
        .get_objs_and_deps(|obj| {
            for (_, id) in &tracks {
                if obj.id() == *id {
                    return true;
                }
            }
            false
        })
        .unwrap();
    println!("Filtered {} OsmObj.", objects.len());

    for (track_label, track_id) in &tracks {
        let (coordinates, stations) = determine_coordinates_and_stations(&objects, track_id);

        // Write all the coordinates to one csv file, just for debugging.
        use std::io::prelude::*;
        let mut file = File::create(format!("{}.csv", track_label)).unwrap();
        let mut counter: usize = 0;
        for (lat, lon) in &coordinates {
            file.write_all(format!("{}, {}, {}\n", lat, lon, counter).as_bytes())
                .unwrap();
            counter += 1;
        }

        let reference_track = ReferenceTrack {
            label: track_label,
            coordinates: coordinates,
            stations: stations,
        };

        let file = File::create(format!("{}.json", track_label)).unwrap();
        serde_json::to_writer(file, &reference_track).unwrap();
    }
}
