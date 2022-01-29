use crate::{
    abc_wrappers::{DiatonicPitchClass, MaybeAccidental, MusicSymbol},
    accidentals::{AccidentalTracker, KeySignatureMap},
    errors::{InfoFieldMissing, PitchConversionError, Result},
    key_signatures::{key_signature, key_signature_for_major, PitchClass},
    midly_wrappers::{MidiMessage, Smf, Track},
};
use abc_parser::datatypes::{
    Accidental::{DoubleFlat, DoubleSharp, Flat, Sharp},
    InfoField, MusicLine as AbcMusicLine,
    MusicSymbol::{
        self as AbcMusicSymbol, Bar as AbcBar, Chord as AbcChord, Ending as AbcEnding,
        GraceNotes as AbcGraceNotes, Note, Rest as AbcRest, Tuplet as AbcTuplet,
        VisualBreak as AbcVisualBreak,
    },
    Note::{A, B, C, D, E, F, G},
    Rest as AbcRestEnum, Tune as AbcTune, TuneBody as AbcTuneBody,
};
use midly::{
    num::{u28, u4, u7},
    Format, Header,
    MetaMessage::{self, EndOfTrack, Tempo, Text, TimeSignature, TrackName},
    MidiMessage::{NoteOff, NoteOn},
    Timing, TrackEvent,
    TrackEventKind::{self, Meta},
};
use std::error::Error;

struct MomentNote {
    key: u7,
    tie: bool,
}

enum Sound {
    Note(MomentNote),
    Chord(Vec<MomentNote>),
    Rest,
}

pub struct Moment {
    pub ticks: u28,
    kind: Sound,
    vel: u7,
}

struct TieTracker([u32; 128]);

impl TieTracker {
    fn new() -> Self {
        Self([0; 128])
    }

    fn index(key: u7) -> usize {
        usize::from(key.as_int())
    }

    fn is_not_tied(&self, key: u7) -> bool {
        self.0[Self::index(key)] == 0
    }

    fn stretch(&mut self, key: u7, ticks: u28) {
        self.0[Self::index(key)] += ticks.as_int();
    }

    fn duration(&self, key: u7, adjustment: i64) -> u28 {
        let d = i64::from(self.0[Self::index(key)]) + adjustment;
        u28::from(d as u32)
    }

    fn reset(&mut self, key: u7) {
        self.0[Self::index(key)] = 0;
    }
}

impl Track<'_> {
    fn symbols_into_events(
        symbols: &[AbcMusicSymbol],
        events: &mut Vec<TrackEvent>,
        channel: u4,
        key_signature: &KeySignatureMap,
    ) -> Result<()> {
        let mut accidental_tracker = AccidentalTracker::new(key_signature);
        let mut moments: Vec<Moment> = vec![];
        Track::symbols_into_moments(symbols, &mut moments, &mut accidental_tracker)?;
        Track::moments_into_events(moments, events, channel)
    }

    fn interpret_symbol(
        visual_symbol: MusicSymbol,
        accidental_tracker: &mut AccidentalTracker,
    ) -> Result<Sound> {
        match visual_symbol.0 {
            // Examples of notes: C ^D _E F2 G2/3
            Note {
                accidental, tie, ..
            } => {
                let played_symbol: MusicSymbol;
                if accidental.is_some() {
                    // Keep accidental for note, and remember it for next notes
                    accidental_tracker.insert(&visual_symbol)?;
                    played_symbol = visual_symbol;
                } else {
                    played_symbol = accidental_tracker.apply(&visual_symbol)?;
                };

                // This is where note conversion happens:
                let key: u7 = played_symbol.try_into()?;
                Ok(Sound::Note(MomentNote {
                    key,
                    tie: tie.is_some(),
                }))
            }
            AbcRest { .. } => Ok(Sound::Rest),
            _ => Err(Box::new(PitchConversionError)),
        }
    }

    pub fn symbols_into_moments<'a>(
        symbols: impl IntoIterator<Item = &'a AbcMusicSymbol>,
        moments: &mut Vec<Moment>,
        accidental_tracker: &mut AccidentalTracker,
    ) -> Result<()> {
        let mut time = 0u32;
        for symbol in symbols {
            let visual_symbol = MusicSymbol(symbol.clone());
            match visual_symbol.0 {
                // Examples of notes: C ^D _E F2 G2/3
                Note { length, .. } => {
                    let ticks = Self::time_into_ticks(length);
                    moments.push(Moment {
                        ticks,
                        kind: Track::interpret_symbol(visual_symbol, accidental_tracker)?,
                        vel: match time {
                            0 => 105.into(),
                            960 => 95.into(),
                            _ => 80.into(),
                        },
                    });
                    time += u32::from(ticks);
                }
                // Examples of chords: [C^D] [_EF2]
                AbcChord { notes, length, .. } => {
                    let ticks = Self::time_into_ticks(length);
                    let mut chord: Vec<MomentNote> = vec![];
                    for symbol in notes {
                        if let Sound::Note(note) =
                            Track::interpret_symbol(MusicSymbol(symbol), accidental_tracker)?
                        {
                            chord.push(note);
                        }
                    }
                    moments.push(Moment {
                        ticks,
                        kind: Sound::Chord(chord),
                        vel: match time {
                            0 => 105.into(),
                            960 => 95.into(),
                            _ => 80.into(),
                        },
                    });
                    time += u32::from(ticks);
                }
                // Barline, notated using the | symbol
                AbcBar(_) => {
                    // Forget accidentals before entering the next bar.
                    accidental_tracker.clear();
                    time = 0;
                }
                AbcRest(rest) => {
                    let rest_length = match rest {
                        AbcRestEnum::Note(length) | AbcRestEnum::NoteHidden(length) => {
                            // Work-around for https://gitlab.com/Askaholic/rust-abc-2/-/issues/2
                            // Only rests corresponding to integer multiples of quarter notes are supported.
                            length
                        }
                        AbcRestEnum::Measure(length) | AbcRestEnum::MeasureHidden(length) => {
                            // TODO: fix for other time signatures than 4/4
                            length * 4
                        }
                    };
                    let ticks = Self::time_into_ticks(rest_length as f32);
                    moments.push(Moment {
                        ticks,
                        kind: Sound::Rest,
                        vel: 0.into(),
                    });
                    time += u32::from(ticks);
                }
                AbcVisualBreak => (),
                AbcEnding(_) | AbcGraceNotes { .. } | AbcTuplet { .. } => todo!(),
            }
        }
        Ok(())
    }

    fn moments_into_events(
        moments: Vec<Moment>,
        events: &mut Vec<TrackEvent>,
        channel: u4,
    ) -> Result<()> {
        let mut ties = TieTracker::new();
        for Moment { ticks, kind, vel } in moments {
            match kind {
                Sound::Note(MomentNote { key, tie }) => {
                    if ties.is_not_tied(key) {
                        events.push(TrackEvent {
                            delta: 1.into(),
                            kind: TrackEventKind::Midi {
                                channel,
                                message: NoteOn { key, vel },
                            },
                        });
                    }
                    ties.stretch(key, ticks);
                    if !tie {
                        events.push(TrackEvent {
                            delta: ties.duration(key, -1),
                            kind: TrackEventKind::Midi {
                                channel,
                                message: NoteOff { key, vel: 0.into() },
                            },
                        });
                        ties.reset(key)
                    }
                }
                Sound::Chord(keys) => {
                    for (index, &MomentNote { key, .. }) in keys.iter().enumerate() {
                        if ties.is_not_tied(key) {
                            events.push(TrackEvent {
                                delta: ((1 + 9 * index) as u32).into(),
                                kind: TrackEventKind::Midi {
                                    channel,
                                    message: NoteOn { key, vel },
                                },
                            })
                        }
                        ties.stretch(key, ticks)
                    }
                    for (index, &MomentNote { key, tie }) in keys.iter().rev().enumerate() {
                        if !tie {
                            events.push(TrackEvent {
                                delta: match index {
                                    0 => (ticks.as_int() + 7 - 9 * keys.len() as u32).into(),
                                    _ => 0.into(),
                                },
                                kind: TrackEventKind::Midi {
                                    channel,
                                    message: NoteOff { key, vel: 0.into() },
                                },
                            });
                            ties.reset(key)
                        }
                    }
                }
                Sound::Rest => {}
            }
        }
        Ok(())
    }
}

impl<'a> Smf<'a> {
    fn get_info_field(info: &'a [InfoField], c: char, default: Option<&'a str>) -> Result<&'a str> {
        match info.iter().find(|&f| f.0 == c) {
            Some(InfoField(_char, string)) => Ok(string),
            None => match default {
                Some(s) => Ok(s),
                None => Err(Box::new(InfoFieldMissing(c))),
            },
        }
    }
}

// Key signature parsing (for major keys only for now):
//                 "F#" (in ABC source)
// 'F'                          '#'
// DiatonicPitchClass(Note::F)  MaybeAccidental(Some(Accidental(Sharp)))
// PitchClass(6)                false (`flat` argument for `key_signature_for_major`)
//                 MetaMessage::KeySignature(6, minor: false)
//                 [1, 1, 1, 1, 1, 1, 0]
fn parse_abc_key_signature_to_midi(info_field_k: &str) -> Result<midly::MetaMessage> {
    let note: DiatonicPitchClass = info_field_k.chars().next().unwrap().try_into()?;
    let accidental: MaybeAccidental = info_field_k.chars().nth(1).try_into()?;
    let flat = accidental == MaybeAccidental(Some(Flat));
    let root_key = PitchClass::from_note_and_accidental(note, accidental);
    let standard_key_signature = key_signature_for_major(root_key, flat);
    Ok(standard_key_signature)
}

impl<'a> TryFrom<&'a AbcTune> for Smf<'a> {
    type Error = Box<dyn Error>;

    fn try_from(value: &'a AbcTune) -> Result<Self> {
        let title = Smf::get_info_field(&value.header.info, 'T', None)?;
        let info_field_k = Smf::get_info_field(&value.header.info, 'K', Some("C"))?;
        let midi_key_signature = parse_abc_key_signature_to_midi(info_field_k)?;
        let body = &value.body;
        let smf: Smf = Smf::try_from((title, midi_key_signature, body))?;
        Ok(smf)
    }
}

fn get_front_matter<'a>(
    title: &'a str,
    key_signature: MetaMessage<'a>,
) -> Result<Vec<TrackEvent<'a>>> {
    Ok(vec![
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
            kind: Meta(key_signature),
        },
        TrackEvent {
            delta: 0.into(),
            kind: Meta(TimeSignature(4, 2, 48, 8)),
        },
        TrackEvent {
            delta: 0.into(),
            kind: Meta(TrackName(title.as_bytes())),
        },
    ])
}

impl<'a> TryFrom<(&'a str, MetaMessage<'a>, &Option<AbcTuneBody>)> for Smf<'a> {
    type Error = Box<dyn Error>;

    fn try_from(value: (&'a str, MetaMessage<'a>, &Option<AbcTuneBody>)) -> Result<Self> {
        let (title, midi_key_signature, maybe_music) = value;
        let key_signature_map = key_signature(midi_key_signature)?;
        let mut smf: Smf = Smf::new(Header::new(
            Format::SingleTrack,
            Timing::Metrical(480.into()),
        ));
        let mut first_track = get_front_matter(title, midi_key_signature)?;
        let mut other_tracks: Vec<Vec<TrackEvent>> = vec![];
        if let Some(AbcTuneBody { music }) = maybe_music {
            let mut tracks: Vec<Vec<TrackEvent>> = music
                .iter()
                .map(|music_line| {
                    let line_with_info = (title, &key_signature_map, music_line);
                    let Track(track) = line_with_info.try_into().unwrap();
                    track
                })
                .collect();
            first_track.append(&mut tracks.remove(0));
            other_tracks.append(&mut tracks);
        }
        first_track.push(TrackEvent {
            delta: 26.into(),
            kind: Meta(EndOfTrack),
        });
        smf.0.tracks.push(first_track);
        smf.0.tracks.append(&mut other_tracks);
        Ok(smf)
    }
}

impl<'a> TryFrom<(&str, &KeySignatureMap, &AbcMusicLine)> for Track<'a> {
    type Error = Box<dyn Error>;

    /// Converts an ABC line of music to a track of MIDI events
    fn try_from(value: (&str, &KeySignatureMap, &AbcMusicLine)) -> Result<Self> {
        let (_title, key_signature_map, music_line) = value;
        let mut events: Vec<TrackEvent> = vec![];
        let channel = u4::from(0);
        Self::symbols_into_events(&music_line.symbols, &mut events, channel, key_signature_map)?;
        Ok(Track(events))
    }
}

impl TryFrom<MusicSymbol> for u7 {
    type Error = Box<dyn Error>;

    fn try_from(value: MusicSymbol) -> Result<Self> {
        match value.0 {
            AbcMusicSymbol::Note {
                accidental,
                note,
                octave,
                ..
            } => {
                let n: i8 = (&DiatonicPitchClass(note)).into();
                let a: i8 = MaybeAccidental(accidental).into();
                let midi_note: u8 = (12 * (octave + 4) + n + a).try_into()?;
                Ok(midi_note.into())
            }
            _ => Err(Box::new(PitchConversionError)),
        }
    }
}

impl TryFrom<MusicSymbol> for MidiMessage {
    type Error = Box<dyn Error>;

    fn try_from(value: MusicSymbol) -> Result<Self> {
        Ok(MidiMessage(NoteOn {
            key: value.try_into()?,
            vel: 80.into(),
        }))
    }
}

impl From<&DiatonicPitchClass> for i8 {
    fn from(note: &DiatonicPitchClass) -> Self {
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

impl From<MaybeAccidental> for i8 {
    fn from(accidental: MaybeAccidental) -> Self {
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
        abc_wrappers::{DiatonicPitchClass, MaybeAccidental, MusicSymbol},
        accidentals::KeySignatureMap,
        midly_wrappers::{MidiMessage, Track},
    };
    use abc_parser::{
        abc,
        datatypes::{
            Accidental::{self as AbcAccidental, DoubleFlat, DoubleSharp, Flat, Natural, Sharp},
            MusicSymbol::{self as AbcMusicSymbol, Note},
            Note::{self as AbcNoteName, A, B, C, D, E, F, G},
        },
    };
    use midly::{num::u7, MidiMessage::NoteOn, TrackEvent, TrackEventKind::Midi};
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
        let music_line = abc::music_line(music).unwrap();
        let title = "title";
        let key_signature_map = KeySignatureMap::new();
        let track = (title, &key_signature_map, &music_line).try_into().unwrap();
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
        let key_signature_map = KeySignatureMap::new();
        Track::symbols_into_events(symbols, &mut events, 1.into(), &key_signature_map).unwrap();
        let pitches: Vec<i8> = events.iter().filter_map(extract_note_on).collect();
        assert_eq!(pitches, &[61, 61]);
    }

    #[rstest(
        abc_symbol,
        expect,
        case(
            Note { note: C, accidental: None, octave: -4, decorations: vec![], length: 1.0, tie: None },
            0,
        ),
        case(
            Note { note: F, accidental: Some(DoubleSharp), octave: 6, decorations: vec![], length: 1.0, tie: None },
            127,
        ),
        case(
            Note { note: D, accidental: Some(DoubleFlat), octave: -1, decorations: vec![], length: 1.0, tie: None },
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
            Note { note: C, accidental: None, octave: 1, decorations: vec![], length: 1.0, tie: None },
            NoteOn { key: 60.into(), vel: 80.into() },
        ),
        case(
            Note { note: B, accidental: Some(Sharp), octave: 0, decorations: vec![], length: 1.0, tie: None },
            NoteOn { key: 60.into(), vel: 80.into() },
        ),
        case(
            Note { note: D, accidental: Some(DoubleFlat), octave: -1, decorations: vec![], length: 1.0, tie: None },
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
        let note = &DiatonicPitchClass(note_name);
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
        let accidental = MaybeAccidental(abc_accidental);
        let semitones: i8 = accidental.into();
        assert_eq!(semitones, expect);
    }
}
