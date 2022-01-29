use crate::errors::{InvalidKeySignatureAccidental, PitchConversionError, Result};
use abc_parser::datatypes::{
    Accidental::{self as AbcAccidental, Flat, Sharp},
    MusicSymbol as AbcMusicSymbol, Note,
};
use std::{error::Error, hash::Hash};

#[derive(Clone, Debug, PartialEq)]
pub struct MusicSymbol(pub AbcMusicSymbol);

impl MusicSymbol {
    pub fn new(symbol: AbcMusicSymbol) -> Self {
        Self(symbol)
    }

    pub fn new_note(
        decorations: Vec<abc_parser::datatypes::Decoration>,
        accidental: MaybeAccidental,
        note: DiatonicPitchClass,
        octave: i8,
        length: f32,
        tie: Option<abc_parser::datatypes::Tie>,
    ) -> Self {
        Self(AbcMusicSymbol::new_note(
            decorations,
            accidental.0,
            note.0,
            octave,
            length,
            tie,
        ))
    }

    pub fn octave(&self) -> Result<i8> {
        match self.0 {
            AbcMusicSymbol::Note { octave, .. } => Ok(octave),
            _ => Err(Box::new(PitchConversionError)),
        }
    }

    pub fn note(&self) -> Result<DiatonicPitchClass> {
        match self.0 {
            AbcMusicSymbol::Note { note, .. } => Ok(DiatonicPitchClass(note)),
            _ => Err(Box::new(PitchConversionError)),
        }
    }

    pub fn accidental(&self) -> Result<MaybeAccidental> {
        match self.0 {
            AbcMusicSymbol::Note { accidental, .. } => Ok(MaybeAccidental(accidental)),
            _ => Err(Box::new(PitchConversionError)),
        }
    }
}

/// Hashabe wrapper for `abc_parser::datatypes::Note`
#[derive(Debug, Eq)]
pub struct DiatonicPitchClass(pub Note);

impl PartialEq for DiatonicPitchClass {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Hash for DiatonicPitchClass {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // abc_parser::datatypes::Note is not hashable, hash MIDI note instead
        let midi_key: i8 = self.into();
        midi_key.hash(state);
    }
}

impl TryFrom<char> for DiatonicPitchClass {
    type Error = Box<dyn Error>;

    /// Converts a pitch name to a `Note` object
    fn try_from(value: char) -> Result<Self> {
        match value {
            'C' => Ok(Self(Note::C)),
            'D' => Ok(Self(Note::D)),
            'E' => Ok(Self(Note::E)),
            'F' => Ok(Self(Note::F)),
            'G' => Ok(Self(Note::G)),
            'A' => Ok(Self(Note::A)),
            'B' => Ok(Self(Note::B)),
            _ => Err(Box::new(PitchConversionError)),
        }
    }
}

impl TryFrom<usize> for DiatonicPitchClass {
    type Error = Box<dyn Error>;

    fn try_from(value: usize) -> Result<Self> {
        match value {
            0 => Ok(Self(Note::C)),
            1 => Ok(Self(Note::D)),
            2 => Ok(Self(Note::E)),
            3 => Ok(Self(Note::F)),
            4 => Ok(Self(Note::G)),
            5 => Ok(Self(Note::A)),
            6 => Ok(Self(Note::B)),
            _ => Err(Box::new(PitchConversionError)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct MaybeAccidental(pub Option<AbcAccidental>);

impl From<MaybeAccidental> for Option<AbcAccidental> {
    fn from(accidental: MaybeAccidental) -> Self {
        accidental.0
    }
}

impl TryFrom<Option<char>> for MaybeAccidental {
    type Error = Box<dyn Error>;

    fn try_from(value: Option<char>) -> Result<Self> {
        match value {
            Some('#') => Ok(Self(Some(Sharp))),
            Some('b') => Ok(Self(Some(Flat))),
            None => Ok(Self(None)),
            Some(c) => Err(Box::new(InvalidKeySignatureAccidental(c))),
        }
    }
}
