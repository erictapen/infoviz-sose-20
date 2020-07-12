#[macro_use]
extern crate serde;

use osmpbfreader::objects::{Ref, RelationId};
use osmpbfreader::{OsmId, OsmObj, Relation, Way};
use std::fs::File;

#[derive(Serialize)]
struct ReferenceTrack {
    label: String,
    coordinates: Vec<(f64, f64)>,
    stations: Vec<(String, (f64, f64))>,
}

fn main() {
    let trackId = OsmId::Relation(RelationId(178663));
    let raw_file = File::open("./raw/brandenburg-latest.osm.pbf").unwrap();
    let mut pbf = osmpbfreader::OsmPbfReader::new(&raw_file);
    let objects = pbf
        .get_objs_and_deps(|obj| {
            obj.id() == trackId // Tram 96
        })
        .unwrap();
    println!("Filtered {} OsmObj.", objects.len());
    let ways: Vec<&OsmObj> = {
        let mut res = Vec::new();
        if let OsmObj::Relation(relation) = objects.get(&trackId).unwrap() {
            for reference in &relation.refs {
                match reference {
                    Ref {
                        member: OsmId::Way(id),
                        ..
                    } => res.push(objects.get(&OsmId::Way(*id)).unwrap()),
                    _ => {}
                }
            }
        }
        res
    };
    println!("{:?}", ways);
}
