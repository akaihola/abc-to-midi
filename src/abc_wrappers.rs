use crate::errors::PitchConversionError;
use abc_parser::datatypes::{
    Accidental::{self as AbcAccidental},
    MusicSymbol as AbcMusicSymbol, Note as AbcNote,
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
        accidental: Accidental,
        note: Note,
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

    pub fn octave(&self) -> Result<i8, Box<dyn Error>> {
        match self.0 {
            AbcMusicSymbol::Note { octave, .. } => Ok(octave),
            _ => Err(Box::new(PitchConversionError)),
        }
    }

    pub fn note(&self) -> Result<Note, Box<dyn Error>> {
        match self.0 {
            AbcMusicSymbol::Note { note, .. } => Ok(Note(note)),
            _ => Err(Box::new(PitchConversionError)),
        }
    }

    pub fn accidental(&self) -> Result<Accidental, Box<dyn Error>> {
        match self.0 {
            AbcMusicSymbol::Note { accidental, .. } => Ok(Accidental(accidental)),
            _ => Err(Box::new(PitchConversionError)),
        }
    }
}

#[derive(Debug, Eq)]
pub struct Note(pub AbcNote);

impl PartialEq for Note {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Hash for Note {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // abc_parser::datatypes::Note is not hashable, hash MIDI note instead
        let midi_key: i8 = self.into();
        midi_key.hash(state);
    }
}

#[derive(Debug, PartialEq)]
pub struct Accidental(pub Option<AbcAccidental>);

impl From<Accidental> for Option<AbcAccidental> {
    fn from(accidental: Accidental) -> Self {
        accidental.0
    }
}
