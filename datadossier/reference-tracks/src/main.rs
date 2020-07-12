#[macro_use]
extern crate serde;

use geo::prelude::*;
use geo::{point, Point};
use osmpbfreader::objects::{Ref, RelationId, WayId};
use osmpbfreader::{OsmId, OsmObj, Relation, Way};
use std::fs::File;
use std::io::prelude::*;

#[derive(Serialize, Debug)]
struct ReferenceTrack<'a> {
    label: &'a str,
    coordinates: Vec<(f64, f64)>,
    stations: Vec<(String, (f64, f64))>,
}

fn main() {
    let track_id = OsmId::Relation(RelationId(178663));
    let start_way = OsmId::Way(WayId(545749156));
    let raw_file = File::open("./raw/brandenburg-latest.osm.pbf").unwrap();
    let mut pbf = osmpbfreader::OsmPbfReader::new(&raw_file);
    let objects = pbf
        .get_objs_and_deps(|obj| {
            obj.id() == track_id // Tram 96
        })
        .unwrap();
    println!("Filtered {} OsmObj.", objects.len());
    let mut coordinates: Vec<(f64, f64)> = {
        let mut res = Vec::new();
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
                                        res.push((node.lat(), node.lon()));
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        res
    };
    {
        let mut sorted_coords: Vec<(f64, f64)> = Vec::new();
        let (mut cursor_lat, mut cursor_lon): (f64, f64) = coordinates.pop().unwrap();
        while !coordinates.is_empty() {
            let mut shortest_dist: (f64, usize) = (f64::MAX, 0);
            let mut cursor_point: Point<f64> = point!(x: cursor_lon, y: cursor_lat);
            for i in 0..coordinates.len() {
                let (lat, lon): (f64, f64) = *coordinates.get(i).unwrap();
                let dist: f64 = cursor_point.geodesic_distance(&point!(x: lon, y: lat));
                if shortest_dist.0 > dist {
                    shortest_dist = (dist, i);
                    cursor_lat = lat;
                    cursor_lon = lon;
                }
            }
            if shortest_dist.0 < f64::MAX {
                println!("shortest dist: {}", shortest_dist.0);
                sorted_coords.push((cursor_lat, cursor_lon));
                coordinates.remove(shortest_dist.1);
            } else {
                break;
            }
        }
        let mut file = File::create("track.csv").unwrap();
        let mut counter: usize = 0;
        for (lat, lon) in &sorted_coords {
            counter += 1;
            file.write_all(format!("{}, {}, {}\n", lat, lon, counter).as_bytes())
                .unwrap();
        }
    }
    let reference_track = ReferenceTrack {
        label: "",
        coordinates: coordinates,
        stations: Vec::new(),
    };
    println!("{}", serde_json::to_string(&reference_track).unwrap());
}
