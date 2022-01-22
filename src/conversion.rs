use crate::{
    abc_wrappers::{Accidental, MusicSymbol, Note},
    accidentals::AccidentalTracker,
    errors::PitchConversionError,
    midly_wrappers::{MidiMessage, Smf, Track},
};
use abc_parser::datatypes::{
    Accidental::{DoubleFlat, DoubleSharp, Flat, Sharp},
    MusicLine as AbcMusicLine,
    MusicSymbol::{
        self as AbcMusicSymbol, Bar as AbcBar, Chord as AbcChord, Ending as AbcEnding,
        GraceNotes as AbcGraceNotes, Note as AbcNote, Rest as AbcRest, Tuplet as AbcTuplet,
        VisualBreak as AbcVisualBreak,
    },
    Note::{A, B, C, D, E, F, G},
    Rest as AbcRestEnum, Tune as AbcTune, TuneBody as AbcTuneBody,
};
use midly::{
    num::{u28, u4, u7},
    Format, Header,
    MetaMessage::{EndOfTrack, KeySignature, Tempo, Text, TimeSignature, TrackName},
    MidiMessage::{NoteOff, NoteOn},
    Timing, TrackEvent,
    TrackEventKind::{Meta, Midi},
};
use std::error::Error;

enum Mode {
    Sequence,
    Chord,
}

impl Track<'_> {
    fn symbols_into_events(
        symbols: &[AbcMusicSymbol],
        events: &mut Vec<TrackEvent>,
        channel: u4,
        mode: Mode,
        prev_length: f32,
        accidental_tracker: &mut AccidentalTracker,
    ) -> Result<(), Box<dyn Error>> {
        let mut prev_length: f32 = prev_length;
        for (idx, symbol) in symbols.iter().enumerate() {
            let visual_symbol = MusicSymbol(symbol.clone());
            let delta: u28 = match (&mode, idx == 0) {
                // All subsequent symbols in a chord occur at the same moment
                // as the first symbol.
                (Mode::Chord, false) => 0.into(),
                // Sequential symbols and first symbols of chords occur when the
                // previous symbol ends, i.e. the delta is the previous symbol's duration.
                _ => 1.into(),
            };
            match visual_symbol.0 {
                // Examples of notes: C ^D _E F2 G2/3
                AbcNote {
                    length, accidental, ..
                } => {
                    let played_symbol = if accidental.is_some() {
                        // Keep accidental for note, and remember it for next notes
                        accidental_tracker.insert(&visual_symbol)?;
                        visual_symbol
                    } else {
                        // No accidental on note, apply remembered one
                        accidental_tracker.apply(&visual_symbol)?
                    };

                    // This is where note conversion happens:
                    let key: u7 = played_symbol.try_into()?;
                    let note_on = MidiMessage(NoteOn {
                        key,
                        vel: match idx {
                            0 => 105,
                            _ => 80,
                        }
                        .into(),
                    });
                    let note_off = MidiMessage(NoteOff { key, vel: 0.into() });

                    // Add a MIDI event corresponding to the ABC note
                    events.push(TrackEvent {
                        delta,
                        kind: Midi {
                            channel,
                            message: note_on.0,
                        },
                    });
                    events.push(TrackEvent {
                        delta: (Self::time_into_ticks(length).as_int() - 1).into(),
                        kind: Midi {
                            channel,
                            message: note_off.0,
                        },
                    });
                    prev_length = length;
                }
                // Examples of chords: [C^D] [_EF2]
                AbcChord { notes, length, .. } => {
                    Self::symbols_into_events(
                        &notes,
                        events,
                        channel,
                        Mode::Chord,
                        prev_length,
                        accidental_tracker,
                    )?;
                    prev_length = length;
                }
                // Barline, notated using the | symbol
                AbcBar(_) => {
                    // Forget accidentals before entering the next bar.
                    accidental_tracker.clear();
                }
                AbcRest(rest) => {
                    match rest {
                        AbcRestEnum::Note(length) | AbcRestEnum::NoteHidden(length) => {
                            // Work-around for https://gitlab.com/Askaholic/rust-abc-2/-/issues/2
                            // Only rests corresponding to integer multiples of quarter notes are supported.
                            prev_length += length as f32
                        }
                        AbcRestEnum::Measure(length) | AbcRestEnum::MeasureHidden(length) => {
                            // TODO: fix for other time signatures than 4/4
                            prev_length += length as f32 * 4.0
                        }
                    }
                }
                AbcVisualBreak => (),
                AbcEnding(_) | AbcGraceNotes { .. } | AbcTuplet { .. } => todo!(),
            }
        }
        Ok(())
    }
}

impl<'a> TryFrom<AbcTune> for Smf<'a> {
    type Error = Box<dyn Error>;

    fn try_from(value: AbcTune) -> Result<Self, Self::Error> {
        let body = value.body.unwrap();
        let smf: Smf = body.try_into()?;
        Ok(smf)
    }
}

fn get_front_matter() -> Vec<TrackEvent<'static>> {
    vec![
        TrackEvent {
            delta: 0.into(),
            kind: Meta(Text("note track".as_bytes())),
        },
        TrackEvent {
            delta: 0.into(),
            kind: Meta(Tempo(500000.into())),
        },
        TrackEvent {
            delta: 0.into(),
            kind: Meta(KeySignature(0, false)),
        },
        TrackEvent {
            delta: 0.into(),
            kind: Meta(TimeSignature(4, 2, 48, 8)),
        },
        TrackEvent {
            delta: 0.into(),
            kind: Meta(TrackName("Three quarter notes".as_bytes())),
        },
    ]
}

impl<'a> TryFrom<AbcTuneBody> for Smf<'a> {
    type Error = Box<dyn Error>;

    fn try_from(value: AbcTuneBody) -> Result<Self, Self::Error> {
        let mut smf: Smf = Smf::new(Header::new(
            Format::SingleTrack,
            Timing::Metrical(480.into()),
        ));
        let mut tracks: Vec<Vec<TrackEvent>> = value
            .music
            .iter()
            .map(|ml| {
                let x: Track = ml.clone().try_into().unwrap();
                x.0
            })
            .collect();
        let mut first_track = get_front_matter();
        first_track.append(&mut tracks.remove(0));
        first_track.push(TrackEvent {
            delta: 26.into(),
            kind: Meta(EndOfTrack),
        });
        smf.0.tracks.push(first_track);
        smf.0.tracks.append(&mut tracks);
        Ok(smf)
    }
}

impl TryFrom<AbcMusicLine> for Track<'_> {
    type Error = Box<dyn Error>;

    fn try_from(value: AbcMusicLine) -> Result<Self, Self::Error> {
        let mut events: Vec<TrackEvent> = vec![];
        let mut accidental_tracker = AccidentalTracker::new();
        let channel = u4::from(0);
        let prev_length = 0.0f32;
        Self::symbols_into_events(
            &value.symbols,
            &mut events,
            channel,
            Mode::Sequence,
            prev_length,
            &mut accidental_tracker,
        )?;
        Ok(Track(events))
    }
}

impl TryFrom<MusicSymbol> for u7 {
    type Error = Box<dyn Error>;

    fn try_from(value: MusicSymbol) -> Result<Self, Box<dyn Error>> {
        match value.0 {
            AbcMusicSymbol::Note {
                accidental,
                note,
                octave,
                ..
            } => {
                let n: i8 = (&Note(note)).into();
                let a: i8 = Accidental(accidental).into();
                let midi_note: u8 = (12 * (octave + 4) + n + a).try_into()?;
                Ok(midi_note.into())
            }
            _ => Err(Box::new(PitchConversionError)),
        }
    }
}

impl TryFrom<MusicSymbol> for MidiMessage {
    type Error = Box<dyn Error>;

    fn try_from(value: MusicSymbol) -> Result<Self, Self::Error> {
        Ok(MidiMessage(NoteOn {
            key: value.try_into()?,
            vel: 80.into(),
        }))
    }
}

impl From<&Note> for i8 {
    fn from(note: &Note) -> Self {
        match note.0 {
            C => 0,
            D => 2,
            E => 4,
            F => 5,
            G => 7,
            A => 9,
            B => 11,
        }
    }
}

impl From<Accidental> for i8 {
    fn from(accidental: Accidental) -> Self {
        match accidental.0 {
            Some(DoubleFlat) => -2,
            Some(Flat) => -1,
            Some(Sharp) => 1,
            Some(DoubleSharp) => 2,
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        abc_wrappers::{Accidental, MusicSymbol, Note},
        accidentals::AccidentalTracker,
        conversion::Mode::Sequence,
        midly_wrappers::{MidiMessage, Track},
    };
    use abc_parser::{
        abc,
        datatypes::{
            Accidental::{self as AbcAccidental, DoubleFlat, DoubleSharp, Flat, Natural, Sharp},
            MusicSymbol::{self as AbcMusicSymbol, Note as AbcNote},
            Note::{self as AbcNoteName, A, B, C, D, E, F, G},
        },
    };
    use midly::{
        num::u7, MidiMessage::NoteOn, TrackEvent, TrackEventKind::Midi,
    };
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    fn deltas(track: &Track) -> Vec<u32> {
        track.iter().map(extract_delta).collect()
    }

    fn extract_delta(event: &TrackEvent) -> u32 {
        event.delta.as_int()
    }

    fn note_ons_and_offs(track: &Track) -> Vec<i8> {
        track.iter().filter_map(extract_note_on).collect()
    }

    fn extract_note_on(event: &TrackEvent) -> Option<i8> {
        if let TrackEvent {
            kind:
                Midi {
                    message: NoteOn { key, vel },
                    ..
                },
            ..
        } = event
        {
            Some(if *vel == 0 {
                -(key.as_int() as i8)
            } else {
                key.as_int() as i8
            })
        } else {
            None
        }
    }

    #[rstest(
        music, expect_deltas, expect_notes,
        case("C", &[1, 239], &[60]),
        case("C4 | D2 | E1", &[1, 959, 1, 479, 1, 239], &[60, 62, 64]),
        case("CDE", &[1, 239, 1, 239, 1, 239], &[60, 62, 64]),
        case("^C C | C", &[1, 239, 1, 239, 1, 239], &[61, 61, 60]),
        case("C z G2 | x2 C'2", &[1, 239, 1, 479, 1, 479], &[60, 67, 72]),
    )]
    fn music_line_try_into_track(music: &str, expect_deltas: &[u32], expect_notes: &[i8]) {
        let track = abc::music_line(music).unwrap().try_into().unwrap();
        assert_eq!(deltas(&track), expect_deltas);
        assert_eq!(note_ons_and_offs(&track), expect_notes);
    }

    #[test]
    fn symbols_into_events() {
        let symbols = &[
            AbcMusicSymbol::new_note(vec![], Some(Sharp), C, 1, 1.0, None),
            AbcMusicSymbol::new_note(vec![], None, C, 1, 1.0, None),
        ];
        let mut events = vec![];
        let mut accidental_tracker = AccidentalTracker::new();
        Track::symbols_into_events(
            symbols,
            &mut events,
            1.into(),
            Sequence,
            1.0,
            &mut accidental_tracker,
        )
        .unwrap();
        let pitches: Vec<i8> = events.iter().filter_map(extract_note_on).collect();
        assert_eq!(pitches, &[61, 61]);
    }

    #[rstest(
        abc_symbol,
        expect,
        case(
            AbcNote { note: C, accidental: None, octave: -4, decorations: vec![], length: 1.0, tie: None },
            0,
        ),
        case(
            AbcNote { note: F, accidental: Some(DoubleSharp), octave: 6, decorations: vec![], length: 1.0, tie: None },
            127,
        ),
        case(
            AbcNote { note: D, accidental: Some(DoubleFlat), octave: -1, decorations: vec![], length: 1.0, tie: None },
            36,
        ),
    )]
    fn musicsymbol_try_into_u7(abc_symbol: AbcMusicSymbol, expect: u8) {
        let pitch: u7 = MusicSymbol(abc_symbol).try_into().unwrap();
        assert_eq!(pitch, u7::from(expect));
    }

    #[rstest(
        abc_symbol,
        expect,
        case(
            AbcNote { note: C, accidental: None, octave: 1, decorations: vec![], length: 1.0, tie: None },
            NoteOn { key: 60.into(), vel: 80.into() },
        ),
        case(
            AbcNote { note: B, accidental: Some(Sharp), octave: 0, decorations: vec![], length: 1.0, tie: None },
            NoteOn { key: 60.into(), vel: 80.into() },
        ),
        case(
            AbcNote { note: D, accidental: Some(DoubleFlat), octave: -1, decorations: vec![], length: 1.0, tie: None },
            NoteOn { key: 36.into(), vel: 80.into() },
        ),
    )]
    fn musicsymbol_try_into_midimessage(abc_symbol: AbcMusicSymbol, expect: midly::MidiMessage) {
        let message: MidiMessage = MusicSymbol(abc_symbol).try_into().unwrap();
        assert_eq!(message, MidiMessage(expect));
    }

    #[rstest(
        note_name,
        expect,
        case(C, 0),
        case(D, 2),
        case(E, 4),
        case(F, 5),
        case(G, 7),
        case(A, 9),
        case(B, 11)
    )]
    fn note_into_i8(note_name: AbcNoteName, expect: i8) {
        let note = &Note(note_name);
        let pitch: i8 = note.into();
        assert_eq!(pitch, expect);
    }

    #[rstest(
        abc_accidental, expect,
        case(None, 0),
        case(Some(Natural), 0),
        case(Some(Sharp), 1),
        case(Some(Flat), -1),
        case(Some(DoubleSharp), 2),
        case(Some(DoubleFlat), -2),
    )]
    fn accidental_into_i8(abc_accidental: Option<AbcAccidental>, expect: i8) {
        let accidental = Accidental(abc_accidental);
        let semitones: i8 = accidental.into();
        assert_eq!(semitones, expect);
    }
}
