use crate::{
    abc_wrappers::{Accidental, MusicSymbol, Note},
    accidentals::AccidentalTracker,
    errors::{InfoFieldMissing, PitchConversionError, Result},
    midly_wrappers::{MidiMessage, Smf, Track},
};
use abc_parser::{
    abc,
    datatypes::{
        Accidental::{DoubleFlat, DoubleSharp, Flat, Sharp},
        InfoField, MusicLine as AbcMusicLine,
        MusicSymbol::{
            self as AbcMusicSymbol, Bar as AbcBar, Chord as AbcChord, Ending as AbcEnding,
            GraceNotes as AbcGraceNotes, Note as AbcNote, Rest as AbcRest, Tuplet as AbcTuplet,
            VisualBreak as AbcVisualBreak,
        },
        Note::{A, B, C, D, E, F, G},
        Rest as AbcRestEnum, Tune as AbcTune, TuneBody as AbcTuneBody,
    },
};
use midly::{
    num::{u28, u4, u7},
    Format, Header,
    MetaMessage::{EndOfTrack, KeySignature, Tempo, Text, TimeSignature, TrackName},
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
    ) -> Result<()> {
        let mut accidental_tracker = AccidentalTracker::new();
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
            AbcNote {
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
                AbcNote { length, .. } => {
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

struct InfoFields<'a> {
    title: &'a str,
    key_signature: &'a str,
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

impl<'a> TryFrom<&'a AbcTune> for Smf<'a> {
    type Error = Box<dyn Error>;

    fn try_from(value: &'a AbcTune) -> Result<Self> {
        let info_fields = InfoFields {
            title: Smf::get_info_field(&value.header.info, 'T', None)?,
            key_signature: Smf::get_info_field(&value.header.info, 'K', Some("C"))?,
        };
        let body = &value.body;
        let smf: Smf = Smf::try_from((info_fields, body))?;
        Ok(smf)
    }
}

fn get_front_matter(info: InfoFields) -> Result<Vec<TrackEvent>> {
    let key_sig_note = &Note(abc::note_uppercase(info.key_signature)?);
    let key_sig_num: i8 = key_sig_note.into();

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
            kind: Meta(KeySignature(key_sig_num, false)),
        },
        TrackEvent {
            delta: 0.into(),
            kind: Meta(TimeSignature(4, 2, 48, 8)),
        },
        TrackEvent {
            delta: 0.into(),
            kind: Meta(TrackName(info.title.as_bytes())),
        },
    ])
}

impl<'a> TryFrom<(InfoFields<'a>, &Option<AbcTuneBody>)> for Smf<'a> {
    type Error = Box<dyn Error>;

    fn try_from(value: (InfoFields<'a>, &Option<AbcTuneBody>)) -> Result<Self> {
        let (info, maybe_music): (InfoFields<'a>, &Option<AbcTuneBody>) = value;
        let mut smf: Smf = Smf::new(Header::new(
            Format::SingleTrack,
            Timing::Metrical(480.into()),
        ));
        let mut first_track = get_front_matter(info)?;
        let mut other_tracks: Vec<Vec<TrackEvent>> = vec![];
        if let Some(AbcTuneBody { music }) = maybe_music {
            let mut tracks: Vec<Vec<TrackEvent>> = music
                .iter()
                .map(|ml| {
                    let x: Track = ml.clone().try_into().unwrap();
                    x.0
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

impl TryFrom<AbcMusicLine> for Track<'_> {
    type Error = Box<dyn Error>;

    fn try_from(value: AbcMusicLine) -> Result<Self> {
        let mut events: Vec<TrackEvent> = vec![];
        let channel = u4::from(0);
        Self::symbols_into_events(&value.symbols, &mut events, channel)?;
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

    fn try_from(value: MusicSymbol) -> Result<Self> {
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
        Track::symbols_into_events(symbols, &mut events, 1.into()).unwrap();
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
