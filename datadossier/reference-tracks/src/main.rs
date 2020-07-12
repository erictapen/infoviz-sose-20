#[macro_use]
extern crate serde;

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
    let coordinates: Vec<(f64, f64)> = {
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
        let mut file = File::create("track.csv").unwrap();
        for coord in &coordinates {
            file.write_all(format!("{}, {}\n", coord.0, coord.1).as_bytes())
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
