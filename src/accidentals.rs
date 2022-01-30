use crate::abc_wrappers::{DiatonicPitchClass, MaybeAccidental, MusicSymbol};
use abc_parser::datatypes::{
    Accidental,
    MusicSymbol::{self as AbcMusicSymbol, Chord as AbcChord, GraceNotes as AbcGraceNotes, Note},
};
use anyhow::{bail, Result};
use std::collections::HashMap;

// Need to use `abc_wrappers::Note` since `abc_parser::datatypes::Note` is not hashable
pub type KeySignatureMap = HashMap<DiatonicPitchClass, Accidental>;

type Octave = i8; // ABC octaves -4..6 cover midi notes 1..127
type AccidentalMap = HashMap<(Octave, DiatonicPitchClass), Accidental>;

#[derive(Debug, PartialEq)]
pub struct AccidentalTracker<'a> {
    accidental_map: AccidentalMap,
    key_signature_map: &'a KeySignatureMap,
}

// Accidental tracking currently is hard-coded to work in the `octave` mode
// as described in the ABC standard v2.1:
// https://abcnotation.com/wiki/abc:standard:v2.1#accidental_directives
impl<'a> AccidentalTracker<'a> {
    pub fn new(key_signature_map: &'a KeySignatureMap) -> Self {
        let accidental_map = AccidentalMap::new();
        Self {
            accidental_map,
            key_signature_map,
        }
    }

    pub fn insert(&mut self, symbol: &MusicSymbol) -> Result<()> {
        if let Ok(MaybeAccidental(Some(v))) = symbol.accidental() {
            self.accidental_map
                .insert((symbol.octave()?, symbol.diatonic_pitch_class()?), v);
        }
        Ok(())
    }

    /// Applies currently active accidentals to a note, chord or grace notes
    pub fn apply(&self, symbol: &MusicSymbol) -> Result<MusicSymbol> {
        match symbol.0.clone() {
            Note {
                decorations,
                accidental: note_accidental,
                note,
                octave,
                length,
                tie,
                ..
            } => {
                let key = (octave, DiatonicPitchClass(note));
                let effective_accidental: Option<Accidental> = match self.accidental_map.get(&key) {
                    // were there earlier accidentals on the pitch in this bar?
                    Some(a) => Some(*a),
                    None => match note_accidental {
                        // no, does the note have its own accidental?
                        Some(a) => Some(a),
                        None => self
                            // no, is there an accidental for the pitch in the key signature?
                            .key_signature_map
                            .get(&DiatonicPitchClass(note))
                            .copied(),
                    },
                };
                Ok(MusicSymbol::new(AbcMusicSymbol::new_note(
                    decorations,
                    effective_accidental,
                    note,
                    octave,
                    length,
                    tie,
                )))
            }
            AbcChord {
                decorations,
                notes,
                length,
            } => Ok(MusicSymbol(AbcChord {
                decorations,
                notes: notes
                    .iter()
                    .map(|note| self.apply(&MusicSymbol(note.clone())).unwrap().0)
                    .collect(),
                length,
            })),
            AbcGraceNotes {
                acciaccatura,
                notes,
            } => Ok(MusicSymbol(AbcGraceNotes {
                acciaccatura,
                notes: notes
                    .iter()
                    .map(|note| self.apply(&MusicSymbol(note.clone())).unwrap().0)
                    .collect(),
            })),
            _ => bail!("Can't apply accidentals to {:?}, expected an abc_parser::MusicSymbol::Note, ::Chord or ::GraceNotes", symbol.0),
        }
    }

    pub fn clear(&mut self) {
        self.accidental_map.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        abc_wrappers::{DiatonicPitchClass, MaybeAccidental, MusicSymbol},
        accidentals::AccidentalTracker,
    };
    use abc_parser::datatypes::{
        Accidental::{self as Accidental, DoubleFlat, DoubleSharp, Flat, Natural, Sharp},
        MusicSymbol::{
            Bar as AbcBar, Chord as AbcChord, Ending as AbcEnding, GraceNotes as AbcGraceNotes,
            Rest as AbcRest, Tuplet as AbcTuplet, VisualBreak as AbcVisualBreak,
        },
        Note::{self as NoteName, A, B, C, D, E, F, G},
    };
    use rstest::rstest;

    type RegisteredAccidental<'a> = &'a (i8, NoteName, Accidental);

    impl<'a> AccidentalTracker<'a> {
        fn register_from_iter<I: IntoIterator<Item = RegisteredAccidental<'a>>>(
            &mut self,
            iter: I,
        ) {
            for (octave, note, accidental) in iter {
                self.accidental_map
                    .insert((*octave, DiatonicPitchClass(*note)), *accidental);
            }
        }
    }

    fn note(octave: i8, pitch: NoteName, accidental: Option<Accidental>) -> MusicSymbol {
        MusicSymbol::new_note(
            vec![],
            MaybeAccidental(accidental),
            DiatonicPitchClass(pitch),
            octave,
            1.0,
            None,
        )
    }

    fn extract_registered_accidentals(tracker: AccidentalTracker) -> Vec<String> {
        let mut accidentals = tracker
            .accidental_map
            .iter()
            .map(|((octave, DiatonicPitchClass(note)), accidental)| {
                format!("{octave} {note:?} {accidental:?}")
            })
            .collect::<Vec<String>>();
        accidentals.sort();
        accidentals
    }

    #[rstest(
        accidentals, expect,
        case(&[], &[]),
        case(&[(0, C, Sharp)], &["0 C Sharp"]),
        case(&[(1, D, Sharp), (1, D, Sharp)], &["1 D Sharp"]),
        case(&[(-1, E, Sharp), (1, E, Sharp)], &["-1 E Sharp", "1 E Sharp"]),
        case(&[(2, F, Sharp), (2, F, Natural)], &["2 F Natural"]),
        case(&[(-2, G, Natural), (-2, G, Sharp), (-2, G, DoubleFlat), (-2, G, Flat)], &["-2 G Flat"]),
    )]
    fn register(accidentals: &[(i8, NoteName, Accidental)], expect: &[&str]) {
        let keysigmap = KeySignatureMap::new();
        let mut tracker = AccidentalTracker::new(&keysigmap);
        for (octave, note, accidental) in accidentals {
            tracker
                .insert(&MusicSymbol::new_note(
                    vec![],
                    MaybeAccidental(Some(*accidental)),
                    DiatonicPitchClass(*note),
                    *octave,
                    1.2,
                    None,
                ))
                .unwrap();
        }
        let result = extract_registered_accidentals(tracker);
        assert_eq!(result, expect);
    }

    #[rstest(
        accidentals, symbol, expect,
        case::different_octave(&[(0, C, Sharp)], note(1, C, None), note(1, C, None)),
        case::flatten_d_sharp_1(&[(1, D, Flat)], note(1, D, Some(Sharp)), note(1, D, Some(Flat))),
        case::double_sharpen_e_minus_1(&[(-1, E, DoubleSharp)], note(-1, E, None), note(-1, E, Some(DoubleSharp))),
        case::double_flatten_f_natural_2(&[(2, F, DoubleFlat)], note(2, F, Some(Natural)), note(2, F, Some(DoubleFlat))),
        case::neutralize_g_flat_minus_2(&[(-2, G, Natural)], note(-2, G, Some(Flat)), note(-2, G, Some(Natural))),
        case::far_away_octave(&[(3, A, Sharp), (4, A, Flat)], note(-3, A, None), note(-3, A, None)),
        case::flatten_b_double_flat_minus_3(&[(-3, B, Flat), (-4, C, DoubleSharp)], note(-3, B, Some(DoubleFlat)), note(-3, B, Some(Flat))),
        case::double_sharpen_grace_c_5(
            &[(5, C, DoubleSharp)],
            MusicSymbol(AbcGraceNotes{ acciaccatura: None, notes: vec![note(5, C, None).0] }),
            MusicSymbol(AbcGraceNotes{ acciaccatura: None, notes: vec![note(5, C, Some(DoubleSharp)).0] })
        ),
        case::double_flatten_chord_d_flat_minus_5(
            &[(-5, D, DoubleFlat)],
            MusicSymbol(AbcChord{ decorations: vec![], notes: vec![note(-5, D, Some(Flat)).0], length: 1.0 }),
            MusicSymbol(AbcChord{ decorations: vec![], notes: vec![note(-5, D, Some(DoubleFlat)).0], length: 1.0 }),
        )
    )]
    fn apply(accidentals: &[(i8, NoteName, Accidental)], symbol: MusicSymbol, expect: MusicSymbol) {
        let keysigmap = KeySignatureMap::new();
        let mut tracker = AccidentalTracker::new(&keysigmap);
        tracker.register_from_iter(accidentals.iter());
        let result = tracker.apply(&symbol).unwrap();
        assert_eq!(result, expect);
    }

    #[rstest(
        symbol,
        case(MusicSymbol(AbcBar("foo".into()))),
        case(MusicSymbol(AbcEnding(0))),
        case(MusicSymbol(AbcRest(abc_parser::datatypes::Rest::Note(0)))),
        case(MusicSymbol(AbcTuplet{ p: 0, q: 0, r: 0, notes: vec![] })),
        case(MusicSymbol(AbcVisualBreak)),
    )]
    fn apply_non_note(symbol: MusicSymbol) {
        let keysigmap = KeySignatureMap::new();
        let tracker = AccidentalTracker::new(&keysigmap);
        let error: anyhow::Error = tracker.apply(&symbol).unwrap_err();
        assert!(matches!(error, anyhow::Error { .. }));
        let expect_message = format!("Can't apply accidentals to {:?}, expected an abc_parser::MusicSymbol::Note, ::Chord or ::GraceNotes", symbol.0);
        assert_eq!(format!("{error:?}"), expect_message);
    }

    #[test]
    fn clear() {
        let accidentals = &[(0, C, Sharp), (1, D, Flat)];
        let keysigmap = KeySignatureMap::new();
        let mut tracker = AccidentalTracker::new(&keysigmap);
        tracker.register_from_iter(accidentals.iter());
        tracker.clear();
        assert!(tracker.accidental_map.is_empty());
    }
}
