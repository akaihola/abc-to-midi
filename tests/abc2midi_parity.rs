#[cfg(test)]
use abc_parser::{
    abc,
    datatypes::{MusicLine, MusicSymbol::Bar, TuneBody},
};
use abc_to_midi::{
    accidentals::AccidentalTracker,
    conversion::Moment,
    midly_wrappers::{Smf, Track},
};
use midly::{
    self,
    num::u7,
    Header,
    MetaMessage::{Text, TrackName},
    MidiMessage::{NoteOff, NoteOn},
    TrackEvent, TrackEventKind,
};
use pretty_assertions::assert_eq;
use rstest::rstest;
use std::str::from_utf8;

const NOTE_NAMES: [&str; 12] = [
    "C", "C#", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "B", "H",
];
const NOTE_NAMES_LOWER: [&str; 12] = [
    "c", "c#", "d", "eb", "e", "f", "f#", "g", "ab", "a", "b", "h",
];
const OCTAVES: [(&str, bool); 11] = [
    ("-3", false), //  0
    ("-2", false), // 12
    ("-1", false), // 24
    ("", false),   // 36
    ("", true),    // 48
    ("1", false),  // 60
    ("2", false),  // 72
    ("3", false),  // 84
    ("4", false),  // 96
    ("5", false),  // 108
    ("6", false),  // 120
];

pub fn key_into_note_name(key: u7) -> String {
    let pitch_u8 = key.as_int();
    let pitch_class = (pitch_u8 % 12) as usize;
    let (octave, lower) = OCTAVES[(pitch_u8 / 12) as usize];
    let pitch_symbol: &str = match lower {
        false => NOTE_NAMES[pitch_class],
        true => NOTE_NAMES_LOWER[pitch_class],
    };

    format!("{pitch_symbol}{octave}")
}

const BAR_LENGTH: u32 = 1920;

fn print_midi(smf: &midly::Smf, first_bar_duration: u32) -> Vec<String> {
    let midly::Smf {
        header: Header { format, timing },
        tracks,
    } = smf;
    let mut v = vec![];
    v.push(format!("format: {format:?}"));
    v.push(format!("timing: {timing:?}"));
    for track in tracks {
        v.push(String::from("Track"));
        let mut ticks = BAR_LENGTH - first_bar_duration;
        for &TrackEvent { delta, kind } in track {
            let content = match kind {
                TrackEventKind::Midi { channel, message } => {
                    let msg = match message {
                        NoteOn { key, vel } | NoteOff { key, vel } => {
                            format!(
                                "{:>3}/{:<3} {:3}",
                                key_into_note_name(key),
                                key.as_int(),
                                vel.as_int()
                            )
                        }
                        _ => format!("{message:?}"),
                    };
                    format!("ch{channel:<2} {msg}")
                }
                TrackEventKind::Meta(message) => match message {
                    Text(chars) => format!("Text('{}')", from_utf8(chars).unwrap()),
                    TrackName(chars) => format!("TrackName('{}')", from_utf8(chars).unwrap()),
                    _ => format!("{:?}", message),
                },
                _ => {
                    format!("{:?}", kind)
                }
            };
            v.push(format!("{:>4} ticks {content}", format!("+{}", delta.as_int())));
            ticks += delta.as_int();
            if ticks >= BAR_LENGTH {
                v.push(String::from("---------------------------"));
                ticks -= BAR_LENGTH;
            }
        }
    }
    v
}

#[rstest(
    name,
    case::three_quarters("three-quarters"),
    case::chord("chord"),
    case::two_bars("two-bars"),
    case::accidentals("accidentals"),
    case::untitled_reel("untitled-reel")
)]
fn compare(name: &str) {
    let abc2midi_raw = std::fs::read(format!("test-asset/{name}.mid")).unwrap();
    let abc2midi_output = midly::Smf::parse(&abc2midi_raw).unwrap();

    let abc_example = std::fs::read_to_string(format!("test-asset/{name}.abc")).unwrap();
    let abc_parsed = &abc::tune(&abc_example).unwrap();
    let first_bar_duration = if let Some(TuneBody { music }) = &abc_parsed.body {
        if let Some(MusicLine { symbols }) = music.first() {
            let mut moments: Vec<Moment> = vec![];
            let mut accidental_tracker = AccidentalTracker::new();
            // let first_bar_symbols = symbols.iter().take_while(|&symbol| match symbol {
            //     Bar(_) => false,
            //     _ => true,
            // });
            let first_bar_symbols = symbols
                .iter()
                .take_while(|&symbol| !matches!(&symbol, Bar(_)));
            Track::symbols_into_moments(first_bar_symbols, &mut moments, &mut accidental_tracker)
                .unwrap();
            moments.iter().map(|moment| moment.ticks.as_int()).sum()
        } else {
            0
        }
    } else {
        0
    };
    let Smf(abc_to_midi_output) = abc_parsed.try_into().unwrap();

    assert_eq!(
        print_midi(&abc_to_midi_output, first_bar_duration),
        print_midi(&abc2midi_output, first_bar_duration)
    );
}
