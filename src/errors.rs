use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum AbcParseError {
    #[error("Note name {0} is invalid")]
    InvalidNoteName(char),
    #[error("Note name {0} is invalid")]
    InvalidDiatonicPitchClass(usize),
    #[error("Diatonic pitch class {0} is invalid, expected 0..=6")]
    InfoFieldMissing(char),
    #[error("Invalid accidental {0} in key signature")]
    InvalidKeySignatureAccidental(char),
    #[error("Invalid meta message kind {0}, expected {1}")]
    WrongMidiMetaMessageKind(String, &'static str),
}
