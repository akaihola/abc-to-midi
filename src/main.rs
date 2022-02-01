use abc_parser::{
    abc,
    datatypes::{
        Accidental::{DoubleFlat, DoubleSharp, Flat, Natural, Sharp},
        MusicSymbol,
    },
};
use abc_to_midi::{accidentals::KeySignatureMap, midly_wrappers::Track};
use midly::{MetaMessage::TimeSignature, MidiMessage::NoteOn, TrackEvent, TrackEventKind::Midi};

fn main() {
    let music_line = abc::music_line("!f! ^C[F=AC]3/2[GBD] C[CEG]2").unwrap();
    print_symbols(&music_line.symbols);
    let key_signature_map = KeySignatureMap::new();
    let time_signature = TimeSignature(4, 4, 48, 8);
    let track = Track::try_from_events(&key_signature_map, &time_signature, &music_line).unwrap();
    print_events(track);
}

fn print_symbols(symbols: &[MusicSymbol]) {
    for symbol in symbols {
        match symbol {
            MusicSymbol::Note {
                accidental,
                note,
                octave,
                length: _,
                ..
            } => {
                println!(
                    "{note:?}{}{octave}",
                    match accidental {
                        Some(Sharp) => "#",
                        Some(Flat) => "b",
                        Some(DoubleSharp) => "##",
                        Some(DoubleFlat) => "bb",
                        Some(Natural) => "=",
                        _ => "",
                    },
                )
            }
            MusicSymbol::Chord { notes, .. } => print_symbols(notes),
            _ => (),
        }
    }
}

fn print_events(track: Track) {
    for &TrackEvent { delta, kind } in track.iter() {
        if let Midi {
            message: NoteOn { key, .. },
            ..
        } = kind
        {
            println!("{:5} {:3}", delta, u8::from(key));
        }
    }
}
