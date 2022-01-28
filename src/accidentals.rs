use crate::{
    abc_wrappers::{Accidental, MusicSymbol, Note},
    errors::PitchConversionError,
};
use abc_parser::datatypes::MusicSymbol::{
    self as AbcMusicSymbol, Chord as AbcChord, GraceNotes as AbcGraceNotes, Note as AbcNote,
};
use std::{collections::HashMap, error::Error};

type AccidentalMap = HashMap<(i8, Note), Accidental>;

#[derive(Debug, Default, PartialEq)]
pub struct AccidentalTracker(AccidentalMap);

// Accidental tracking currently is hard-coded to work in the `octave` mode
// as described in the ABC standard v2.1:
// https://abcnotation.com/wiki/abc:standard:v2.1#accidental_directives
impl AccidentalTracker {
    pub fn new() -> Self {
        AccidentalTracker(HashMap::new())
    }

    pub fn insert(&mut self, symbol: &MusicSymbol) -> Result<(), Box<dyn Error>> {
        self.0
            .insert((symbol.octave()?, symbol.note()?), symbol.accidental()?);
        Ok(())
    }

    pub fn apply(&self, symbol: &MusicSymbol) -> Result<MusicSymbol, Box<dyn Error>> {
        match &symbol.0 {
            AbcNote {
                decorations,
                accidental: note_accidental,
                note,
                octave,
                length,
                tie,
                ..
            } => {
                let key = (*octave, Note(*note));
                let acc = self.0.get(&key);
                let bar_accidental = match acc {
                    Some(&Accidental(None)) => note_accidental,
                    Some(&Accidental(Some(_))) => &acc.unwrap().0,
                    None => note_accidental,
                };
                Ok(MusicSymbol::new(AbcMusicSymbol::new_note(
                    decorations.clone(),
                    *bar_accidental,
                    *note,
                    *octave,
                    *length,
                    *tie,
                )))
            }
            AbcChord {
                decorations,
                notes,
                length,
            } => Ok(MusicSymbol(AbcChord {
                decorations: decorations.clone(),
                notes: notes
                    .iter()
                    .map(|note| self.apply(&MusicSymbol(note.clone())).unwrap().0)
                    .collect(),
                length: *length,
            })),
            AbcGraceNotes {
                acciaccatura,
                notes,
            } => Ok(MusicSymbol(AbcGraceNotes {
                acciaccatura: *acciaccatura,
                notes: notes
                    .iter()
                    .map(|note| self.apply(&MusicSymbol(note.clone())).unwrap().0)
                    .collect(),
            })),
            _ => Err(Box::new(PitchConversionError)),
        }
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        abc_wrappers::{Accidental, MusicSymbol, Note},
        accidentals::AccidentalTracker,
        errors::PitchConversionError,
    };
    use abc_parser::datatypes::{
        Accidental::{self as AbcAccidental, DoubleFlat, DoubleSharp, Flat, Natural, Sharp},
        MusicSymbol::{
            Bar as AbcBar, Chord as AbcChord, Ending as AbcEnding, GraceNotes as AbcGraceNotes,
            Rest as AbcRest, Tuplet as AbcTuplet, VisualBreak as AbcVisualBreak,
        },
        Note::{self as NoteName, A, B, C, D, E, F, G},
    };
    use rstest::rstest;

    type RegisteredAccidental<'a> = &'a (i8, NoteName, AbcAccidental);

    impl<'a> FromIterator<RegisteredAccidental<'a>> for AccidentalTracker {
        fn from_iter<I: IntoIterator<Item = RegisteredAccidental<'a>>>(iter: I) -> Self {
            let mut a = Self::new();
            for (octave, note, accidental) in iter {
                a.0.insert((*octave, Note(*note)), Accidental(Some(*accidental)));
            }
            a
        }
    }

    fn note(octave: i8, pitch: NoteName, accidental: Option<AbcAccidental>) -> MusicSymbol {
        MusicSymbol::new_note(
            vec![],
            Accidental(accidental),
            Note(pitch),
            octave,
            1.0,
            None,
        )
    }

    #[rstest(
        accidentals, expect,
        case(&[], &[]),
        case(&[(0, C, Sharp)], &[(0, C, Sharp)]),
        case(&[(1, D, Sharp), (1, D, Sharp)], &[(1, D, Sharp)]),
        case(&[(-1, E, Sharp), (1, E, Sharp)], &[(-1, E, Sharp), (1, E, Sharp)]),
        case(&[(2, F, Sharp), (2, F, Natural)], &[(2, F, Natural)]),
        case(&[(-2, G, Natural), (-2, G, Sharp), (-2, G, DoubleFlat), (-2, G, Flat)], &[(-2, G, Flat)]),
    )]
    fn register(
        accidentals: &[(i8, NoteName, AbcAccidental)],
        expect: &[(i8, NoteName, AbcAccidental)],
    ) {
        let mut bar_accidentals = AccidentalTracker::new();
        for (octave, note, accidental) in accidentals {
            bar_accidentals
                .insert(&MusicSymbol::new_note(
                    vec![],
                    Accidental(Some(*accidental)),
                    Note(*note),
                    *octave,
                    1.2,
                    None,
                ))
                .unwrap();
        }
        let expect_ = expect.iter().collect::<AccidentalTracker>();
        assert_eq!(bar_accidentals, expect_);
    }

    #[rstest(
        accidentals, symbol, expect,
        case(&[(0, C, Sharp)], note(1, C, None), note(1, C, None)),
        case(&[(1, D, Flat)], note(1, D, Some(Sharp)), note(1, D, Some(Flat))),
        case(&[(-1, E, DoubleSharp)], note(-1, E, None), note(-1, E, Some(DoubleSharp))),
        case(&[(2, F, DoubleFlat)], note(2, F, Some(Natural)), note(2, F, Some(DoubleFlat))),
        case(&[(-2, G, Natural)], note(-2, G, Some(Flat)), note(-2, G, Some(Natural))),
        case(&[(3, A, Sharp), (4, A, Flat)], note(-3, A, None), note(-3, A, None)),
        case(&[(-3, B, Flat), (-4, C, DoubleSharp)], note(-3, B, Some(DoubleFlat)), note(-3, B, Some(Flat))),
        case(
            &[(5, C, DoubleSharp)],
            MusicSymbol(AbcGraceNotes{ acciaccatura: None, notes: vec![note(5, C, None).0] }),
            MusicSymbol(AbcGraceNotes{ acciaccatura: None, notes: vec![note(5, C, Some(DoubleSharp)).0] })
        ),
        case(
            &[(-5, D, DoubleFlat)],
            MusicSymbol(AbcChord{ decorations: vec![], notes: vec![note(-5, D, Some(Flat)).0], length: 1.0 }),
            MusicSymbol(AbcChord{ decorations: vec![], notes: vec![note(-5, D, Some(DoubleFlat)).0], length: 1.0 }),
        )
    )]
    fn apply(
        accidentals: &[(i8, NoteName, AbcAccidental)],
        symbol: MusicSymbol,
        expect: MusicSymbol,
    ) {
        let tracker: AccidentalTracker = accidentals.iter().collect::<AccidentalTracker>();
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
        let tracker = AccidentalTracker::new();
        let error = tracker.apply(&symbol).err().unwrap();
        assert!(error.is::<PitchConversionError>());
    }

    #[test]
    fn clear() {
        let accidentals = &[(0, C, Sharp), (1, D, Flat)];
        let mut tracker: AccidentalTracker = accidentals.iter().collect();
        tracker.clear();
        assert!(tracker.0.is_empty());
    }
}
